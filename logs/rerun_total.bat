@echo off
cd /d "%~dp0.."
"C:\Program Files\Git\bin\bash.exe" logs/rerun_total.sh >> logs\rerun_total.out 2>&1
