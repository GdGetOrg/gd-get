namespace GdGet.Abstractions.Attributes;

[AttributeUsage(AttributeTargets.Class, Inherited = false)]
public sealed class CliCommandAttribute : Attribute
{
    public CliCommandAttribute(string name)
    {
        Name = name;
    }

    public string Name { get; }
    public string Description { get; set; } = string.Empty;
}