using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Scriban;

namespace GdGet.Cli.SourceGen;

[Generator(LanguageNames.CSharp)]
public sealed class CliCommandGenerator : IIncrementalGenerator
{
    private const string CLI_COMMAND_ATTRIBUTE_METADATA_NAME = "GdGet.Abstractions.Attributes.CliCommandAttribute";
    private const string CLI_OPTION_ATTRIBUTE_METADATA_NAME = "GdGet.Abstractions.Attributes.CliOptionAttribute";
    private const string CLI_ARGUMENT_ATTRIBUTE_METADATA_NAME = "GdGet.Abstractions.Attributes.CliArgumentAttribute";
    private const string CLI_COMMAND_INTERFACE_METADATA_NAME = "GdGet.Abstractions.Interfaces.ICliCommand";

    private static readonly SymbolDisplayFormat TypeDisplayFormat =
        SymbolDisplayFormat.FullyQualifiedFormat.WithMiscellaneousOptions(
            SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
            SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier);

    private static readonly DiagnosticDescriptor CommandClassShapeRule = new(
        id: "CLI001",
        title: "Invalid command class shape",
        messageFormat: "Command class '{0}' must be declared as 'public partial class' and cannot be static",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor MissingRunMethodRule = new(
        id: "CLI002",
        title: "Invalid Run method signature",
        messageFormat: "Command class '{0}' must declare exactly one public instance Run() method returning int with no parameters",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor InvalidBindingRule = new(
        id: "CLI003",
        title: "Invalid property binding",
        messageFormat: "Property '{0}' must have exactly one binding attribute: [CliOption] or [CliArgument]",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor InvalidOptionRule = new(
        id: "CLI004",
        title: "Invalid option metadata",
        messageFormat: "Property '{0}' has an invalid option name. Option names must start with '-' or '--'.",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor InvalidArgumentRule = new(
        id: "CLI005",
        title: "Invalid argument metadata",
        messageFormat: "Property '{0}' has an invalid argument position. Positions must be 0 or greater.",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor DuplicateArgumentPositionRule = new(
        id: "CLI006",
        title: "Duplicate argument position",
        messageFormat: "Command '{0}' has duplicate [CliArgument] position '{1}'",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor DuplicateOptionNameRule = new(
        id: "CLI007",
        title: "Duplicate option name",
        messageFormat: "Command '{0}' has duplicate [CliOption] name '{1}'",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor ReservedCommandNameRule = new(
        id: "CLI008",
        title: "Reserved command name",
        messageFormat: "Command '{0}' cannot be used because 'help' is reserved for the generated help command",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor DuplicateCommandNameRule = new(
        id: "CLI009",
        title: "Duplicate command name",
        messageFormat: "Command name '{0}' is declared more than once",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor UnsupportedCommandTypeRule = new(
        id: "CLI010",
        title: "Unsupported command type",
        messageFormat: "Command class '{0}' must be a top-level non-generic class",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor InvalidBindingPropertyRule = new(
        id: "CLI011",
        title: "Invalid binding property",
        messageFormat: "Property '{0}' must be writable and cannot be static or an indexer",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor RunMethodParameterRule = new(
        id: "CLI012",
        title: "Run method cannot declare parameters",
        messageFormat: "Run method on command '{0}' cannot declare parameters. Move [CliOption]/[CliArgument] to writable properties.",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor ParameterBindingAttributeRule = new(
        id: "CLI013",
        title: "Binding attributes are not allowed on parameters",
        messageFormat: "Parameter '{0}' uses [CliOption]/[CliArgument], but bindings are only supported on properties",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor MissingCommandInterfaceRule = new(
        id: "CLI014",
        title: "Missing command interface",
        messageFormat: "Command class '{0}' must implement ICliCommand",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor InvalidConstructorRule = new(
        id: "CLI015",
        title: "Invalid constructor shape",
        messageFormat: "Command class '{0}' must have exactly one public instance constructor",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor InvalidConstructorParameterRule = new(
        id: "CLI016",
        title: "Invalid constructor parameter",
        messageFormat: "Constructor parameter '{0}' on command '{1}' cannot be ref/out/in, optional, or params",
        category: "GdGet.Cli.SourceGen",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        IncrementalValuesProvider<CommandCandidate> candidates = context.SyntaxProvider
            .CreateSyntaxProvider(
                static (node, _) => node is ClassDeclarationSyntax classDeclaration && classDeclaration.AttributeLists.Count > 0,
                static (syntaxContext, cancellationToken) => BuildCandidate(syntaxContext, cancellationToken))
            .Where(static candidate => candidate is not null)
            .Select(static (candidate, _) => candidate!);

        context.RegisterSourceOutput(candidates.Collect(), EmitGeneratedSources);
    }

    private static CommandCandidate? BuildCandidate(GeneratorSyntaxContext context, CancellationToken cancellationToken)
    {
        ClassDeclarationSyntax classDeclaration = (ClassDeclarationSyntax)context.Node;

        if (context.SemanticModel.GetDeclaredSymbol(classDeclaration, cancellationToken) is not { } classSymbol)
        {
            return null;
        }

        AttributeData? cliCommandAttribute = GetAttribute(classSymbol, CLI_COMMAND_ATTRIBUTE_METADATA_NAME);
        if (cliCommandAttribute is null)
        {
            return null;
        }

        ImmutableArray<Diagnostic>.Builder diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        Location classLocation = classDeclaration.Identifier.GetLocation();

        bool isPartial = classDeclaration.Modifiers.Any(static modifier => modifier.IsKind(SyntaxKind.PartialKeyword));
        bool isPublic = classSymbol.DeclaredAccessibility == Accessibility.Public;
        if (classSymbol.IsStatic || !isPartial || !isPublic)
        {
            diagnostics.Add(Diagnostic.Create(CommandClassShapeRule, classLocation, classSymbol.Name));
        }

        if (classSymbol.ContainingType is not null || classSymbol.TypeArguments.Length > 0)
        {
            diagnostics.Add(Diagnostic.Create(UnsupportedCommandTypeRule, classLocation, classSymbol.Name));
        }

        bool implementsCliCommandInterface = classSymbol.AllInterfaces.Any(IsCliCommandInterface);
        if (!implementsCliCommandInterface)
        {
            diagnostics.Add(Diagnostic.Create(MissingCommandInterfaceRule, classLocation, classSymbol.Name));
        }

        string? commandName = GetConstructorStringArgument(cliCommandAttribute, 0);
        if (string.IsNullOrWhiteSpace(commandName))
        {
            return new CommandCandidate(null, diagnostics.ToImmutable());
        }

        if (string.Equals(commandName, "help", System.StringComparison.OrdinalIgnoreCase))
        {
            diagnostics.Add(Diagnostic.Create(ReservedCommandNameRule, classLocation, commandName));
        }

        string commandDescription = GetNamedStringArgument(cliCommandAttribute, "Description") ?? string.Empty;

        ImmutableArray<IMethodSymbol> candidateRunMethods = classSymbol
            .GetMembers("Run")
            .OfType<IMethodSymbol>()
            .Where(static method =>
                method.MethodKind == MethodKind.Ordinary &&
                method.DeclaredAccessibility == Accessibility.Public &&
                !method.IsStatic &&
                method.ReturnType.SpecialType == SpecialType.System_Int32)
            .ToImmutableArray();

        ImmutableArray<IMethodSymbol> runMethods =
        [
            ..candidateRunMethods
                .Where(static method => method.Parameters.Length == 0)
        ];

        if (runMethods.Length != 1)
        {
            diagnostics.Add(Diagnostic.Create(MissingRunMethodRule, classLocation, classSymbol.Name));
        }

        foreach (IMethodSymbol runMethodWithParameters in candidateRunMethods.Where(static method => method.Parameters.Length > 0))
        {
            Location runLocation = runMethodWithParameters.Locations.FirstOrDefault() ?? classLocation;
            diagnostics.Add(Diagnostic.Create(RunMethodParameterRule, runLocation, classSymbol.Name));

            foreach (IParameterSymbol parameter in runMethodWithParameters.Parameters)
            {
                AttributeData? optionAttribute = GetAttribute(parameter, CLI_OPTION_ATTRIBUTE_METADATA_NAME);
                AttributeData? argumentAttribute = GetAttribute(parameter, CLI_ARGUMENT_ATTRIBUTE_METADATA_NAME);
                if (optionAttribute is null && argumentAttribute is null)
                {
                    continue;
                }

                Location parameterLocation = parameter.Locations.FirstOrDefault() ?? runLocation;
                diagnostics.Add(Diagnostic.Create(ParameterBindingAttributeRule, parameterLocation, parameter.Name));
            }
        }

        ImmutableArray<IMethodSymbol> publicConstructors =
        [
            ..classSymbol.InstanceConstructors
                .Where(static constructor => constructor.DeclaredAccessibility == Accessibility.Public && !constructor.IsStatic)
                .OrderBy(static constructor => constructor.Locations.FirstOrDefault()?.SourceSpan.Start ?? int.MaxValue)
        ];

        IMethodSymbol? selectedConstructor = null;
        if (publicConstructors.Length != 1)
        {
            diagnostics.Add(Diagnostic.Create(InvalidConstructorRule, classLocation, classSymbol.Name));
        }
        else
        {
            selectedConstructor = publicConstructors[0];
        }

        ImmutableArray<ConstructorParameterDefinition>.Builder constructorParameters = ImmutableArray.CreateBuilder<ConstructorParameterDefinition>();
        if (selectedConstructor is not null)
        {
            foreach (IParameterSymbol parameter in selectedConstructor.Parameters)
            {
                if (parameter.RefKind != RefKind.None || parameter.IsOptional || parameter.IsParams)
                {
                    Location parameterLocation = parameter.Locations.FirstOrDefault() ?? classLocation;
                    diagnostics.Add(Diagnostic.Create(InvalidConstructorParameterRule, parameterLocation, parameter.Name, classSymbol.Name));
                    continue;
                }

                constructorParameters.Add(new ConstructorParameterDefinition(
                    parameter.Name,
                    parameter.Type.ToDisplayString(TypeDisplayFormat)));
            }
        }

        ImmutableArray<IPropertySymbol> classProperties =
        [
            ..classSymbol.GetMembers()
                .OfType<IPropertySymbol>()
                .OrderBy(static property => property.Locations.FirstOrDefault()?.SourceSpan.Start ?? int.MaxValue)
        ];

        ImmutableArray<BindingDefinition>.Builder bindings = ImmutableArray.CreateBuilder<BindingDefinition>();

        foreach (IPropertySymbol property in classProperties)
        {
            AttributeData? optionAttribute = GetAttribute(property, CLI_OPTION_ATTRIBUTE_METADATA_NAME);
            AttributeData? argumentAttribute = GetAttribute(property, CLI_ARGUMENT_ATTRIBUTE_METADATA_NAME);

            if (optionAttribute is null && argumentAttribute is null)
            {
                continue;
            }

            Location propertyLocation = property.Locations.FirstOrDefault() ?? classLocation;

            if (property.IsStatic || property.Parameters.Length > 0 || property.SetMethod is null)
            {
                diagnostics.Add(Diagnostic.Create(InvalidBindingPropertyRule, propertyLocation, property.Name));
                continue;
            }

            if (optionAttribute is not null && argumentAttribute is not null)
            {
                diagnostics.Add(Diagnostic.Create(InvalidBindingRule, propertyLocation, property.Name));
                continue;
            }

            int bindingIndex = bindings.Count;

            if (optionAttribute is not null)
            {
                string optionName = GetConstructorStringArgument(optionAttribute, 0) ?? string.Empty;
                if (string.IsNullOrWhiteSpace(optionName) || !optionName.StartsWith("-", System.StringComparison.Ordinal))
                {
                    diagnostics.Add(Diagnostic.Create(InvalidOptionRule, propertyLocation, property.Name));
                    continue;
                }

                bindings.Add(new BindingDefinition(
                    property.Name,
                    property.Type.ToDisplayString(TypeDisplayFormat),
                    property.Type.IsReferenceType && property.NullableAnnotation != NullableAnnotation.Annotated,
                    BindingKind.Option,
                    optionName,
                    0,
                    GetNamedStringArgument(optionAttribute, "Description") ?? string.Empty,
                    bindingIndex,
                    propertyLocation));
                continue;
            }

            int? argumentPosition = GetConstructorIntArgument(argumentAttribute!, 0);
            if (argumentPosition is null or < 0)
            {
                diagnostics.Add(Diagnostic.Create(InvalidArgumentRule, propertyLocation, property.Name));
                continue;
            }

            string argumentName = GetNamedStringArgument(argumentAttribute!, "Name") ?? string.Empty;
            if (string.IsNullOrWhiteSpace(argumentName))
            {
                argumentName = property.Name;
            }

            bindings.Add(new BindingDefinition(
                property.Name,
                property.Type.ToDisplayString(TypeDisplayFormat),
                property.Type.IsReferenceType && property.NullableAnnotation != NullableAnnotation.Annotated,
                BindingKind.Argument,
                argumentName,
                argumentPosition.Value,
                GetNamedStringArgument(argumentAttribute!, "Description") ?? string.Empty,
                bindingIndex,
                propertyLocation));
        }

        foreach (IGrouping<int, BindingDefinition> duplicatePosition in bindings
                     .Where(static binding => binding.Kind == BindingKind.Argument)
                     .GroupBy(static binding => binding.Position)
                     .Where(static grouping => grouping.Count() > 1))
        {
            foreach (BindingDefinition binding in duplicatePosition)
            {
                diagnostics.Add(Diagnostic.Create(DuplicateArgumentPositionRule, binding.Location, commandName, duplicatePosition.Key));
            }
        }

        foreach (IGrouping<string, BindingDefinition> duplicateOption in bindings
                     .Where(static binding => binding.Kind == BindingKind.Option)
                     .GroupBy(static binding => binding.SymbolName, System.StringComparer.Ordinal)
                     .Where(static grouping => grouping.Count() > 1))
        {
            foreach (BindingDefinition binding in duplicateOption)
            {
                diagnostics.Add(Diagnostic.Create(DuplicateOptionNameRule, binding.Location, commandName, duplicateOption.Key));
            }
        }

        if (diagnostics.Count > 0)
        {
            return new CommandCandidate(null, diagnostics.ToImmutable());
        }

        string namespaceName = classSymbol.ContainingNamespace.IsGlobalNamespace
            ? string.Empty
            : classSymbol.ContainingNamespace.ToDisplayString();

        CommandDefinition commandDefinition = new(
            commandName!,
            commandDescription,
            classSymbol.Name,
            namespaceName,
            classSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat),
            constructorParameters.ToImmutable(),
            bindings.ToImmutable(),
            classLocation);

        return new CommandCandidate(commandDefinition, diagnostics.ToImmutable());
    }

    private static void EmitGeneratedSources(SourceProductionContext context, ImmutableArray<CommandCandidate> candidates)
    {
        foreach (CommandCandidate candidate in candidates)
        {
            foreach (Diagnostic diagnostic in candidate.Diagnostics)
            {
                context.ReportDiagnostic(diagnostic);
            }
        }

        ImmutableArray<CommandDefinition> commands =
        [
            ..candidates
                .Where(static candidate => candidate.Definition is not null)
                .Select(static candidate => candidate.Definition!)
        ];

        ImmutableArray<IGrouping<string, CommandDefinition>> duplicateCommandNames =
        [
            ..commands
                .GroupBy(static command => command.CommandName, System.StringComparer.OrdinalIgnoreCase)
                .Where(static group => group.Count() > 1)
        ];

        if (duplicateCommandNames.Length > 0)
        {
            HashSet<string> duplicateNameSet = new(System.StringComparer.OrdinalIgnoreCase);
            foreach (IGrouping<string, CommandDefinition> duplicateGroup in duplicateCommandNames)
            {
                duplicateNameSet.Add(duplicateGroup.Key);
            }

            foreach (IGrouping<string, CommandDefinition> duplicateGroup in duplicateCommandNames)
            {
                foreach (CommandDefinition command in duplicateGroup)
                {
                    context.ReportDiagnostic(Diagnostic.Create(DuplicateCommandNameRule, command.Location, duplicateGroup.Key));
                }
            }

            commands =
            [
                ..commands
                    .Where(command => !duplicateNameSet.Contains(command.CommandName))
            ];
        }

        ImmutableArray<CommandDefinition> orderedCommands =
        [
            ..commands
                .OrderBy(static command => command.CommandName, System.StringComparer.Ordinal)
        ];

        GeneratedCliRenderModel renderModel = BuildGeneratedCliRenderModel(orderedCommands);
        string generatedCliSource = RenderGeneratedCliTemplate(renderModel);
        context.AddSource("GeneratedCli.g.cs", SourceText.From(generatedCliSource, Encoding.UTF8));
    }

    private static GeneratedCliRenderModel BuildGeneratedCliRenderModel(ImmutableArray<CommandDefinition> orderedCommands)
    {
        ImmutableArray<GeneratedCommandRenderModel> commands =
        [
            ..orderedCommands
                .Select(BuildGeneratedCommandRenderModel)
        ];

        return new GeneratedCliRenderModel(ToLiteral("gd-get"), commands);
    }

    private static GeneratedCommandRenderModel BuildGeneratedCommandRenderModel(CommandDefinition command, int commandIndex)
    {
        string registerVariable = $"command{commandIndex}";
        ImmutableArray<BindingDefinition> optionDefinitions =
        [
            ..command.Bindings
                .Where(static binding => binding.Kind == BindingKind.Option)
                .OrderBy(static binding => binding.BindingIndex)
        ];
        ImmutableArray<BindingDefinition> argumentDefinitions =
        [
            ..command.Bindings
                .Where(static binding => binding.Kind == BindingKind.Argument)
                .OrderBy(static binding => binding.Position)
        ];

        Dictionary<int, GeneratedBindingRenderModel> bindingsByIndex = new();
        ImmutableArray<GeneratedBindingRenderModel>.Builder optionBindings = ImmutableArray.CreateBuilder<GeneratedBindingRenderModel>(optionDefinitions.Length);
        ImmutableArray<GeneratedBindingRenderModel>.Builder argumentBindings = ImmutableArray.CreateBuilder<GeneratedBindingRenderModel>(argumentDefinitions.Length);

        for (int optionIndex = 0; optionIndex < optionDefinitions.Length; optionIndex++)
        {
            BindingDefinition binding = optionDefinitions[optionIndex];
            GeneratedBindingRenderModel renderedBinding = CreateGeneratedBindingRenderModel(binding, $"{registerVariable}Option{optionIndex}");
            bindingsByIndex[binding.BindingIndex] = renderedBinding;
            optionBindings.Add(renderedBinding);
        }

        for (int argumentIndex = 0; argumentIndex < argumentDefinitions.Length; argumentIndex++)
        {
            BindingDefinition binding = argumentDefinitions[argumentIndex];
            GeneratedBindingRenderModel renderedBinding = CreateGeneratedBindingRenderModel(binding, $"{registerVariable}Argument{argumentIndex}");
            bindingsByIndex[binding.BindingIndex] = renderedBinding;
            argumentBindings.Add(renderedBinding);
        }

        ImmutableArray<GeneratedBindingRenderModel> orderedBindings =
        [
            ..command.Bindings
                .OrderBy(static binding => binding.BindingIndex)
                .Select(binding => bindingsByIndex[binding.BindingIndex])
        ];

        string bridgeSignature = BuildBindingSignature(command.Bindings);
        string bridgeSignatureWithServices = string.IsNullOrWhiteSpace(bridgeSignature)
            ? "global::System.IServiceProvider services"
            : $"global::System.IServiceProvider services, {bridgeSignature}";

        string bridgeParameterCall = string.Join(", ", orderedBindings.Select(static binding => binding.BridgeParameterName));
        string bridgeParameterCallWithServices = string.IsNullOrWhiteSpace(bridgeParameterCall)
            ? "services"
            : $"services, {bridgeParameterCall}";

        string invocationArgumentList = string.Join(", ", orderedBindings.Select(static binding => binding.InvocationExpression));
        string invocationArgumentListWithServices = string.IsNullOrWhiteSpace(invocationArgumentList)
            ? "services"
            : $"services, {invocationArgumentList}";

        return new GeneratedCommandRenderModel(
            className: command.ClassName,
            namespaceName: command.NamespaceName,
            commandTypeName: command.CommandTypeName,
            commandNameLiteral: ToLiteral(command.CommandName),
            descriptionLiteral: ToLiteral(command.Description),
            usageLiteral: ToLiteral(BuildCommandUsage(command)),
            registerVariable: registerVariable,
            bridgeSignature: bridgeSignature,
            bridgeSignatureWithServices: bridgeSignatureWithServices,
            bridgeParameterCall: bridgeParameterCall,
            bridgeParameterCallWithServices: bridgeParameterCallWithServices,
            invocationArgumentList: invocationArgumentList,
            invocationArgumentListWithServices: invocationArgumentListWithServices,
            constructorArgumentList: BuildConstructorArgumentList(command),
            optionBindings: optionBindings.ToImmutable(),
            argumentBindings: argumentBindings.ToImmutable(),
            orderedBindings: orderedBindings);
    }

    private static GeneratedBindingRenderModel CreateGeneratedBindingRenderModel(BindingDefinition binding, string declarationVariable)
    {
        string bridgeParameterName = GetBridgeParameterName(binding);
        string valueVariable = $"value{binding.BindingIndex}";
        string nullGuardMessage = $"Missing value for required property '{binding.PropertyName}'.";

        string bridgeAssignmentExpression = binding.IsNonNullableReference
            ? $"{bridgeParameterName} ?? throw new global::System.InvalidOperationException({ToLiteral(nullGuardMessage)})"
            : bridgeParameterName;

        string invocationExpression = binding.IsNonNullableReference
            ? $"{valueVariable} ?? throw new global::System.InvalidOperationException({ToLiteral(nullGuardMessage)})"
            : valueVariable;

        return new GeneratedBindingRenderModel(
            propertyName: binding.PropertyName,
            typeName: binding.TypeName,
            symbolNameLiteral: ToLiteral(binding.SymbolName),
            hasDescription: !string.IsNullOrWhiteSpace(binding.Description),
            descriptionLiteral: ToLiteral(binding.Description),
            declarationVariable: declarationVariable,
            sourceVariable: declarationVariable,
            valueVariable: valueVariable,
            bridgeParameterName: bridgeParameterName,
            bridgeAssignmentExpression: bridgeAssignmentExpression,
            invocationExpression: invocationExpression);
    }

    private static string RenderGeneratedCliTemplate(GeneratedCliRenderModel model)
    {
        Template template = Template.Parse(GENERATED_CLI_TEMPLATE);
        if (!template.HasErrors) return template.Render(model, static member => member.Name);
        string errorMessages = string.Join("; ", template.Messages.Select(static message => message.Message));
        throw new InvalidOperationException($"Failed to parse generated CLI template: {errorMessages}");

    }

    private static string BuildBindingSignature(ImmutableArray<BindingDefinition> bindings)
    {
        if (bindings.Length == 0)
        {
            return string.Empty;
        }

        return string.Join(", ", bindings
            .OrderBy(static binding => binding.BindingIndex)
            .Select(binding => $"{binding.TypeName} {GetBridgeParameterName(binding)}"));
    }

    private static string BuildConstructorArgumentList(CommandDefinition command)
    {
        if (command.ConstructorParameters.Length == 0)
        {
            return string.Empty;
        }

        return string.Join(", ", command.ConstructorParameters.Select(parameter =>
        {
            string missingServiceMessage =
                $"Missing required service '{parameter.TypeName}' for constructor parameter '{parameter.ParameterName}' on command '{command.CommandName}'.";
            return
                $"({parameter.TypeName})(services.GetService(typeof({parameter.TypeName})) ?? throw new global::System.InvalidOperationException({ToLiteral(missingServiceMessage)}))";
        }));
    }

    private static string BuildCommandUsage(CommandDefinition command)
    {
        StringBuilder usageBuilder = new(command.CommandName);

        if (command.Bindings.Any(static binding => binding.Kind == BindingKind.Option))
        {
            usageBuilder.Append(" [options]");
        }

        foreach (BindingDefinition argumentBinding in command.Bindings
                     .Where(static binding => binding.Kind == BindingKind.Argument)
                     .OrderBy(static binding => binding.Position))
        {
            usageBuilder.Append(" <");
            usageBuilder.Append(argumentBinding.SymbolName);
            usageBuilder.Append('>');
        }

        return usageBuilder.ToString();
    }

    private static string GetBridgeParameterName(BindingDefinition binding)
    {
        string candidate = binding.PropertyName.Length == 1
            ? binding.PropertyName.ToLowerInvariant()
            : char.ToLowerInvariant(binding.PropertyName[0]) + binding.PropertyName.Substring(1);
        return SyntaxFacts.GetKeywordKind(candidate) == SyntaxKind.None ? candidate : "@" + candidate;
    }


    private static AttributeData? GetAttribute(ISymbol symbol, string attributeMetadataName)
    {
        return symbol.GetAttributes().FirstOrDefault(attribute =>
            string.Equals(attribute.AttributeClass?.ToDisplayString(), attributeMetadataName, System.StringComparison.Ordinal));
    }

    private static bool IsCliCommandInterface(INamedTypeSymbol interfaceSymbol)
    {
        return interfaceSymbol is { TypeKind: TypeKind.Interface } &&
               string.Equals(interfaceSymbol.ToDisplayString(), CLI_COMMAND_INTERFACE_METADATA_NAME, System.StringComparison.Ordinal);
    }

    private static string? GetConstructorStringArgument(AttributeData attributeData, int index)
    {
        if (attributeData.ConstructorArguments.Length <= index)
        {
            return null;
        }

        TypedConstant typedConstant = attributeData.ConstructorArguments[index];
        return typedConstant.Value as string;
    }

    private static int? GetConstructorIntArgument(AttributeData attributeData, int index)
    {
        if (attributeData.ConstructorArguments.Length <= index)
        {
            return null;
        }

        TypedConstant typedConstant = attributeData.ConstructorArguments[index];
        return typedConstant.Value is int intValue ? intValue : null;
    }

    private static string? GetNamedStringArgument(AttributeData attributeData, string argumentName)
    {
        foreach (KeyValuePair<string, TypedConstant> namedArgument in attributeData.NamedArguments)
        {
            if (!string.Equals(namedArgument.Key, argumentName, System.StringComparison.Ordinal))
            {
                continue;
            }

            return namedArgument.Value.Value as string;
        }

        return null;
    }

    private static string ToLiteral(string value)
    {
        StringBuilder builder = new(value.Length + 2);
        builder.Append('"');

        foreach (char character in value)
        {
            switch (character)
            {
                case '\\':
                    builder.Append(@"\\");
                    break;
                case '"':
                    builder.Append("\\\"");
                    break;
                case '\r':
                    builder.Append(@"\r");
                    break;
                case '\n':
                    builder.Append(@"\n");
                    break;
                case '\t':
                    builder.Append(@"\t");
                    break;
                default:
                    builder.Append(character);
                    break;
            }
        }

        builder.Append('"');
        return builder.ToString();
    }

    private const string GENERATED_CLI_TEMPLATE = """
        // <auto-generated />
        #nullable enable
        using Spectre.Console;
        
        {{~ for command in Commands ~}}
        {{~ if command.HasNamespace ~}}
        namespace {{ command.NamespaceName }}
        {
        {{~ end ~}}
        public partial class {{ command.ClassName }}
        {
            internal static int __GeneratedInvoke({{ command.BridgeSignatureWithServices }})
            {
                if (services is null)
                {
                    throw new global::System.ArgumentNullException(nameof(services));
                }
        
        {{~ if command.HasBindings ~}}
                var command = new {{ command.ClassName }}({{ command.ConstructorArgumentList }})
                {
        {{~ for binding in command.OrderedBindings ~}}
                    {{ binding.PropertyName }} = {{ binding.BridgeAssignmentExpression }},
        {{~ end ~}}
                };
        {{~ else ~}}
                var command = new {{ command.ClassName }}({{ command.ConstructorArgumentList }});
        {{~ end ~}}
        
                return command.Run();
            }
        {{~ if command.HasBindings ~}}
        
            public static int Run({{ command.BridgeSignatureWithServices }})
            {
                return __GeneratedInvoke({{ command.BridgeParameterCallWithServices }});
            }
        {{~ end ~}}
        }
        {{~ if command.HasNamespace ~}}
        }
        {{~ end ~}}
        
        {{~ end ~}}
        internal static class GeneratedCli
        {
            public static void Register(global::System.CommandLine.RootCommand root, global::System.IServiceProvider services)
            {
                if (root is null)
                {
                    throw new global::System.ArgumentNullException(nameof(root));
                }
        
                if (services is null)
                {
                    throw new global::System.ArgumentNullException(nameof(services));
                }
        
        {{~ for command in Commands ~}}
                var {{ command.RegisterVariable }} = new global::System.CommandLine.Command({{ command.CommandNameLiteral }}, {{ command.DescriptionLiteral }});
        {{~ for option in command.OptionBindings ~}}
                var {{ option.DeclarationVariable }} = new global::System.CommandLine.Option<{{ option.TypeName }}>({{ option.SymbolNameLiteral }});
        {{~ if option.HasDescription ~}}
                {{ option.DeclarationVariable }}.Description = {{ option.DescriptionLiteral }};
        {{~ end ~}}
                {{ command.RegisterVariable }}.Options.Add({{ option.DeclarationVariable }});
        {{~ end ~}}
        {{~ for argument in command.ArgumentBindings ~}}
                var {{ argument.DeclarationVariable }} = new global::System.CommandLine.Argument<{{ argument.TypeName }}>({{ argument.SymbolNameLiteral }});
        {{~ if argument.HasDescription ~}}
                {{ argument.DeclarationVariable }}.Description = {{ argument.DescriptionLiteral }};
        {{~ end ~}}
                {{ command.RegisterVariable }}.Arguments.Add({{ argument.DeclarationVariable }});
        {{~ end ~}}
                {{ command.RegisterVariable }}.SetAction(parseResult =>
                {
        {{~ for binding in command.OrderedBindings ~}}
                    var {{ binding.ValueVariable }} = parseResult.GetValue({{ binding.SourceVariable }});
        {{~ end ~}}
                    return {{ command.CommandTypeName }}.__GeneratedInvoke({{ command.InvocationArgumentListWithServices }});
                });
        
                root.Subcommands.Add({{ command.RegisterVariable }});
        
        {{~ end ~}}
                RegisterHelpCommand(root);
            }
        
            public static int InvokeWithAutoHelp(global::System.CommandLine.RootCommand root, string[] args)
            {
                if (root is null)
                {
                    throw new global::System.ArgumentNullException(nameof(root));
                }
        
                if (args is null)
                {
                    throw new global::System.ArgumentNullException(nameof(args));
                }
        
                if (args.Length == 0)
                {
                    ShowHelp(root);
                    return 0;
                }
        
                if (args.Length == 1 && IsRootHelpToken(args[0]))
                {
                    ShowHelp(root);
                    return 0;
                }
        
                if (!IsKnownTopLevelToken(args[0]))
                {
                    global::System.Console.WriteLine($"Unknown command '{args[0]}'.");
                    global::System.Console.WriteLine();
                    ShowHelp(root);
                    return 1;
                }
        
                global::System.CommandLine.ParseResult parseResult = root.Parse(args);
                if (parseResult.Errors.Count > 0)
                {
                    foreach (global::System.CommandLine.Parsing.ParseError parseError in parseResult.Errors)
                    {
                        global::System.Console.WriteLine(parseError.Message);
                    }
        
                    global::System.Console.WriteLine();
                    ShowHelp(root);
                    return 1;
                }
        
                return parseResult.Invoke();
            }
        
            private static bool IsKnownTopLevelToken(string token)
            {
                if (token.StartsWith("-", global::System.StringComparison.Ordinal))
                {
                    return true;
                }
        
                return token switch
                {
                    "help" => true,
        {{~ for command in Commands ~}}
                    {{ command.CommandNameLiteral }} => true,
        {{~ end ~}}
                    _ => false,
                };
            }
        
            private static bool IsRootHelpToken(string token)
            {
                return token is "--help" or "-h" or "-?";
            }
        
            private static void RegisterHelpCommand(global::System.CommandLine.RootCommand root)
            {
                var helpCommand = new global::System.CommandLine.Command("help", "Show help for available commands");
                helpCommand.SetAction(_ =>
                {
                    ShowHelp(root);
                    return 0;
                });
                root.Subcommands.Add(helpCommand);
            }
        
            private static void ShowHelp(global::System.CommandLine.RootCommand root)
            {
                const string appName = {{ AppNameLiteral }};
                string escapedAppName = global::Spectre.Console.Markup.Escape(appName);
                var header = new global::Spectre.Console.Rule($"[bold deepskyblue1]{escapedAppName}[/]");
                header.Justification = global::Spectre.Console.Justify.Left;
                global::Spectre.Console.AnsiConsole.Write(header);
                global::Spectre.Console.AnsiConsole.MarkupLine($"[grey]Usage:[/] [blue]{escapedAppName}[/] [yellow]<command>[/] [grey][[options]][/]");
                global::Spectre.Console.AnsiConsole.WriteLine();
        
                var commandsTable = new global::Spectre.Console.Table();
                commandsTable.Border = global::Spectre.Console.TableBorder.Rounded;
                commandsTable.ShowRowSeparators = true;
                commandsTable.AddColumn("[grey]Command[/]");
                commandsTable.AddColumn("[grey]Usage[/]");
                commandsTable.AddColumn("[grey]Description[/]");
        
        {{~ for command in Commands ~}}
                commandsTable.AddRow(global::Spectre.Console.Markup.Escape({{ command.CommandNameLiteral }}), global::Spectre.Console.Markup.Escape({{ command.UsageLiteral }}), global::Spectre.Console.Markup.Escape({{ command.DescriptionLiteral }}));
        {{~ end ~}}
                commandsTable.AddRow(global::Spectre.Console.Markup.Escape("help"), global::Spectre.Console.Markup.Escape("help"), global::Spectre.Console.Markup.Escape("Show help for available commands"));
        
                global::Spectre.Console.AnsiConsole.Write(commandsTable);
                global::Spectre.Console.AnsiConsole.WriteLine();
                global::Spectre.Console.AnsiConsole.MarkupLine("[grey]Global options:[/] [blue]-h[/], [blue]--help[/], [blue]--version[/]");
            }
        }
        """;

    private sealed class CommandCandidate(CommandDefinition? definition, ImmutableArray<Diagnostic> diagnostics)
    {
        public CommandDefinition? Definition { get; } = definition;
        public ImmutableArray<Diagnostic> Diagnostics { get; } = diagnostics;
    }

    private sealed class CommandDefinition(
        string commandName,
        string description,
        string className,
        string namespaceName,
        string commandTypeName,
        ImmutableArray<ConstructorParameterDefinition> constructorParameters,
        ImmutableArray<BindingDefinition> bindings,
        Location location)
    {
        public string CommandName { get; } = commandName;
        public string Description { get; } = description;
        public string ClassName { get; } = className;
        public string NamespaceName { get; } = namespaceName;
        public string CommandTypeName { get; } = commandTypeName;
        public ImmutableArray<ConstructorParameterDefinition> ConstructorParameters { get; } = constructorParameters;
        public ImmutableArray<BindingDefinition> Bindings { get; } = bindings;
        public Location Location { get; } = location;
    }

    private sealed class GeneratedCliRenderModel(
        string appNameLiteral,
        ImmutableArray<GeneratedCommandRenderModel> commands)
    {
        public string AppNameLiteral { get; } = appNameLiteral;
        public ImmutableArray<GeneratedCommandRenderModel> Commands { get; } = commands;
    }

    private sealed class GeneratedCommandRenderModel
    {
        public GeneratedCommandRenderModel(
            string className,
            string namespaceName,
            string commandTypeName,
            string commandNameLiteral,
            string descriptionLiteral,
            string usageLiteral,
            string registerVariable,
            string bridgeSignature,
            string bridgeSignatureWithServices,
            string bridgeParameterCall,
            string bridgeParameterCallWithServices,
            string invocationArgumentList,
            string invocationArgumentListWithServices,
            string constructorArgumentList,
            ImmutableArray<GeneratedBindingRenderModel> optionBindings,
            ImmutableArray<GeneratedBindingRenderModel> argumentBindings,
            ImmutableArray<GeneratedBindingRenderModel> orderedBindings)
        {
            ClassName = className;
            NamespaceName = namespaceName;
            CommandTypeName = commandTypeName;
            CommandNameLiteral = commandNameLiteral;
            DescriptionLiteral = descriptionLiteral;
            UsageLiteral = usageLiteral;
            RegisterVariable = registerVariable;
            BridgeSignature = bridgeSignature;
            BridgeSignatureWithServices = bridgeSignatureWithServices;
            BridgeParameterCall = bridgeParameterCall;
            BridgeParameterCallWithServices = bridgeParameterCallWithServices;
            InvocationArgumentList = invocationArgumentList;
            InvocationArgumentListWithServices = invocationArgumentListWithServices;
            ConstructorArgumentList = constructorArgumentList;
            OptionBindings = optionBindings;
            ArgumentBindings = argumentBindings;
            OrderedBindings = orderedBindings;
        }

        public string ClassName { get; }
        public string NamespaceName { get; }
        public string CommandTypeName { get; }
        public string CommandNameLiteral { get; }
        public string DescriptionLiteral { get; }
        public string UsageLiteral { get; }
        public string RegisterVariable { get; }
        public string BridgeSignature { get; }
        public string BridgeSignatureWithServices { get; }
        public string BridgeParameterCall { get; }
        public string BridgeParameterCallWithServices { get; }
        public string InvocationArgumentList { get; }
        public string InvocationArgumentListWithServices { get; }
        public string ConstructorArgumentList { get; }
        public ImmutableArray<GeneratedBindingRenderModel> OptionBindings { get; }
        public ImmutableArray<GeneratedBindingRenderModel> ArgumentBindings { get; }
        public ImmutableArray<GeneratedBindingRenderModel> OrderedBindings { get; }
        public bool HasNamespace => !string.IsNullOrWhiteSpace(NamespaceName);
        public bool HasBindings => OrderedBindings.Length > 0;
    }

    private sealed class GeneratedBindingRenderModel
    {
        public GeneratedBindingRenderModel(
            string propertyName,
            string typeName,
            string symbolNameLiteral,
            bool hasDescription,
            string descriptionLiteral,
            string declarationVariable,
            string sourceVariable,
            string valueVariable,
            string bridgeParameterName,
            string bridgeAssignmentExpression,
            string invocationExpression)
        {
            PropertyName = propertyName;
            TypeName = typeName;
            SymbolNameLiteral = symbolNameLiteral;
            HasDescription = hasDescription;
            DescriptionLiteral = descriptionLiteral;
            DeclarationVariable = declarationVariable;
            SourceVariable = sourceVariable;
            ValueVariable = valueVariable;
            BridgeParameterName = bridgeParameterName;
            BridgeAssignmentExpression = bridgeAssignmentExpression;
            InvocationExpression = invocationExpression;
        }

        public string PropertyName { get; }
        public string TypeName { get; }
        public string SymbolNameLiteral { get; }
        public bool HasDescription { get; }
        public string DescriptionLiteral { get; }
        public string DeclarationVariable { get; }
        public string SourceVariable { get; }
        public string ValueVariable { get; }
        public string BridgeParameterName { get; }
        public string BridgeAssignmentExpression { get; }
        public string InvocationExpression { get; }
    }

    private sealed class BindingDefinition
    {
        public BindingDefinition(
            string propertyName,
            string typeName,
            bool isNonNullableReference,
            BindingKind kind,
            string symbolName,
            int position,
            string description,
            int bindingIndex,
            Location location)
        {
            PropertyName = propertyName;
            TypeName = typeName;
            IsNonNullableReference = isNonNullableReference;
            Kind = kind;
            SymbolName = symbolName;
            Position = position;
            Description = description;
            BindingIndex = bindingIndex;
            Location = location;
        }

        public string PropertyName { get; }
        public string TypeName { get; }
        public bool IsNonNullableReference { get; }
        public BindingKind Kind { get; }
        public string SymbolName { get; }
        public int Position { get; }
        public string Description { get; }
        public int BindingIndex { get; }
        public Location Location { get; }
    }

    private enum BindingKind
    {
        Option,
        Argument
    }

    private sealed class ConstructorParameterDefinition(string parameterName, string typeName)
    {
        public string ParameterName { get; } = parameterName;
        public string TypeName { get; } = typeName;
    }
}
