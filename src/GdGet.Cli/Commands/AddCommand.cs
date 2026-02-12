using System.ComponentModel.DataAnnotations;
using GdGet.Abstractions.Attributes;
using GdGet.Abstractions.Interfaces;

namespace GdGet.Cli.Commands;
[CliCommand("add", Description = "Add an addon dependency, update the lockfile, and sync install.")]
public partial class AddCommand: ICliCommand
{
    [CliArgument(0, Name= "addon", Description =  "Addon ID to add and install")]
    public required string AddonName { get; init; }
    
    public int Run()
    {
        Console.WriteLine($"Fetching {AddonName}...");
        Console.WriteLine($"Fetching {AddonName}...");
        Console.WriteLine($"Extracting...");
        return 0;
    }
}
