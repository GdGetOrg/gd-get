using GdGet.Abstractions.Attributes;
using GdGet.Abstractions.Interfaces;

namespace GdGet.Cli.Commands;

[CliCommand("install", Description = "Install addons from the lockfile, or resolve and create it.")]
public partial class InstallCommand: ICliCommand
{
    public int Run()
    {
        Console.WriteLine("Reading dependencies...");
        Console.WriteLine("Fetching...");
        Console.WriteLine("Downloading...");
        Console.WriteLine("Extracting archives...");
        Console.WriteLine("Added to addons folder");
        return 0;
    }
}
