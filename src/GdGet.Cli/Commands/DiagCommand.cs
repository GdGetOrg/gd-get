using GdGet.Abstractions.Attributes;
using GdGet.Abstractions.Interfaces;
using GdGet.Core.Diagnostics;

namespace GdGet.Cli.Commands;

[CliCommand("diag", Description = "Run a diagnostic command that verifies constructor DI.")]
public partial class DiagCommand(ICommandProbeService probeService) : ICliCommand
{
    [CliArgument(0, Name = "input", Description = "Input passed to the diagnostic service")]
    public required string Input { get; init; }

    public int Run()
    {
        Console.WriteLine(probeService.BuildMessage(Input));
        return 0;
    }
}
