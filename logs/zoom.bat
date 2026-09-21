@echo off
cd /d "%~dp0.."
set NICE_USE_BUDGET=1
set NICE_BUDGET_EXACT=1
set NICE_BUDGET_LIMIT=1000
set NICE_BUDGET_START=2025
set NICE_WELFARE_END=2300
rem Resumed after a power cut: level 1 over [0, 0.20] picked B = 0.02 (logs/tu_zoom_budget_level1.log),
rem so levels 2-3 of the same search run over its neighbourhood [0, 0.04].
set NICE_B_MIN=0.0
set NICE_B_MAX=0.04
set NICE_N_ZOOM=2
"C:\Users\fabre\.julia\juliaup\julia-1.12.3+0.x64.w64.mingw32\bin\julia.exe" --heap-size-hint=3G --project=. cap_and_share/find_global_exp_carbon_tax_buget_zoom.jl > logs\tu_zoom.log 2>&1
