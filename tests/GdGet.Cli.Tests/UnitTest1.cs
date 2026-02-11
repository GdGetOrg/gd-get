using GdGet.Cli;

namespace GdGet.Cli.Tests;

public class UnitTest1
{
    [Fact]
    public void Run_WritesHelpForHelpFlag()
    {
        var output = new StringWriter();

        var exitCode = CliApp.Run(["--help"], output);

        Assert.Equal(0, exitCode);
        Assert.Contains("Usage: gd-get", output.ToString());
    }
}
