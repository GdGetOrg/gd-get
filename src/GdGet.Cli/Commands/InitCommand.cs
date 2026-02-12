using GdGet.Abstractions.Attributes;
using GdGet.Abstractions.Interfaces;

namespace GdGet.Cli.Commands;

[CliCommand("init", Description = "Create the gd-get.toml manifest in the current project.")]
public partial class InitCommand: ICliCommand
{
    public int Run()
    {
        Console.WriteLine("project initialized (naaat)");
        return 0;
    }
}
