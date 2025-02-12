## NICE 2020

This code couples together a country version of the NICE model with the [FAIRv2.0 Mimi model](https://github.com/FrankErrickson/MimiFAIRv2.jl).

### Software Requirements
This code was created with Julia v1.9.3 and Mimi v1.5.1. You can find documentation on the Mimi framework at [https://www.mimiframework.org/Mimi.jl/stable/](https://www.mimiframework.org/Mimi.jl/stable/). 
If you need to install Julia, also add to the PATH the location of julia.exe (e.g. C:\Users\your_name\AppData\Local\Programs\Julia-1.11.3\bin).

To automatically download the necessary packages and dependencies in the correct version:
(1) set the NICE2020 repository as root directory using 
```julia
cd("path/to/NICE2020")
```
(2) run the following code: 
```julia
using Pkg
Pkg.activate(joinpath(@__DIR__))
Pkg.instantiate()
```
This will create a Julia environment and install all the necessary packages as described in the Project.toml file. 

Alternatively, you can install each necessary package by entering the Julia package manager through typing the `]` key and then running
```julia
add PackageName
```

To install the `MimiFAIRv2` package, open up the Julia package manager by typing the `]` key. Then run the following code:
```julia
add https://github.com/FrankErrickson/MimiFAIRv2.jl.git
```

### Running The Code
There are two ways to run this code.

First, you can run a set of example runs with different model settings. 

Alternatively, you can run the code for the NICE2020 model using the `MimiNICE2020` module.

#### Running The Example Runs
(1) Set this repository as your working directory.  
(2) Load the file for the example runs. The results will be stored in the NICE2020/results folder.
```julia
include("src/example_runs.jl")
```

#### Running The Model As A Module/Package
(1) Set this repository as your working directory.  
(2) Load the module file to create your model:
```julia
include("src/nice2020_module.jl")
```
(3) Create an instance of this model. By loading the module, it's as if you imported a `create_nice2020` function from one of your Julia packages.
```julia
m = MimiNICE2020.create_nice2020()
```
(4) Run the model.
```julia
run(m)
```
(5) Examine the results. Here we extract the temperature and then look at automatically generated plots of all the model output.
```julia
my_temp = m[:temperature, :T]
explore(m)
```
