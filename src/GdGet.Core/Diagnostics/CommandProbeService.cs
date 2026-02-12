namespace GdGet.Core.Diagnostics;

public sealed class CommandProbeService : ICommandProbeService
{
    public string BuildMessage(string input)
    {
        if (string.IsNullOrWhiteSpace(input))
        {
            return "DI service resolved. No input provided.";
        }

        return $"DI service resolved. Echo: {input}";
    }
}
