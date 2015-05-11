@echo off
set /p version= <buildnum.txt
set /a version=version+1
echo|set /p=%version% >buildnum.txt
brass.exe graph3dc.asm -l graph3dc.html
if errorlevel 1 goto ERRORS
rabbitsign -p -t 8xk -f -g -vv -k 010F.key graph3dc.hex
move graph3dc.8xk Graph3DC.8ck
if errorlevel 1 goto ERRORS
GOTO:EOF

:ERRORS
color 04
echo ----- There were errors.
color 07