@echo off
cd /d "%~dp0.."
"C:\Program Files\Git\bin\bash.exe" logs/rerun_exp.sh >> logs\rerun_exp.out 2>&1
