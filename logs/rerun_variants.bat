@echo off
cd /d "%~dp0.."
"C:\Program Files\Git\bin\bash.exe" logs/rerun_variants.sh >> logs\rerun_variants.out 2>&1
