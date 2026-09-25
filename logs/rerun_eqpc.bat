@echo off
cd /d "%~dp0.."
set NICE_RECYCLING=equal_pc
"C:\Program Files\Git\bin\bash.exe" logs/rerun_eqpc.sh >> logs\rerun_eqpc.out 2>&1
