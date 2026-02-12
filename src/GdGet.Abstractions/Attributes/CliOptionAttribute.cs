namespace GdGet.Abstractions.Attributes;

[AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = false)]
public sealed class CliOptionAttribute : Attribute
{
    public CliOptionAttribute(string name)
    {
        Name = name;
    }

    public string Name { get; }
    public string Description { get; set; } = string.Empty;
}