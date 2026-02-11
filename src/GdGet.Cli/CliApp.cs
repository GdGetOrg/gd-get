namespace GdGet.Cli;

public static class CliApp
{
    public static int Run(string[] args, TextWriter output)
    {
        if (args.Contains("--help") || args.Contains("-h"))
        {
            output.WriteLine("Usage: gd-get [options]");
            output.WriteLine("Options:");
            output.WriteLine("  -h|--help    Show help");
            return 0;
        }

        output.WriteLine("gd-get CLI scaffold");
        return 0;
    }
}
