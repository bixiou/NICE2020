@echo off
cd /d "%~dp0.."
"C:\Program Files\Git\bin\bash.exe" logs/after_zoom.sh >> logs\rerun_exp.out 2>&1
