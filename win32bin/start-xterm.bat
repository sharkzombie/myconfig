@echo on
if %SESSIONNAME% EQU RDP-Tcp#1 goto display1
if %SESSIONNAME% EQU RDP-Tcp#2 goto display1

set DISPLAY=:0.0

goto run

:display1

set DISPLAY=:0.1

:run

C:\cygwin64\bin\run.exe xterm -ls
