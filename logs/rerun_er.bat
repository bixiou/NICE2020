@echo off
cd /d "%~dp0.."
"C:\Program Files\Git\bin\bash.exe" logs/rerun_er.sh >> logs\rerun_er.out 2>&1
