using System.CommandLine;
using GdGet.Core.Diagnostics;
using Microsoft.Extensions.DependencyInjection;

ServiceCollection services = new();
services.AddSingleton<ICommandProbeService, CommandProbeService>();
using ServiceProvider serviceProvider = services.BuildServiceProvider();

RootCommand rootCommand = new("gd-get command line interface");
GeneratedCli.Register(rootCommand, serviceProvider);

return GeneratedCli.InvokeWithAutoHelp(rootCommand, args);
