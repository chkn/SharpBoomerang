@echo off
fsi --exec build.fsx %* || echo Ensure you are in a Visual Studio Command Prompt.
