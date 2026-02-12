using JetBrains.Annotations;

namespace GdGet.Abstractions.Attributes;

[AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = false)]
public sealed class CliArgumentAttribute(int position) : Attribute
{
    [UsedImplicitly]
    public int Position { get; } = position;
    
    [UsedImplicitly]
    public string Name { get; set; } = string.Empty;
    
    [UsedImplicitly]
    public string Description { get; set; } = string.Empty;
}