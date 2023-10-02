#] add BifurcationKit

using Revise, Parameters, Plots
using BifurcationKit
using Setfield

## following tutorial from Bifurcation Analysis documentation
# link: https://bifurcationkit.github.io/BifurcationKitDocs.jl/dev/tutorials/ode/tutorialPP2/#pp2-example-from-AUTO07p-(aBD-Hopf-aBS)

# function to record information from a solution
recordFromSolution(x, p) = (x1 = x[1], x2 = x[2])

function pollution!(dz, z, p, t = 0)
	@unpack u1, u2, s1, s2, v1, v2, d12, d21, z12, z21, alpha = p
	x1, x2 = z
    # equations
    dz[1]= u1 - s1*x1 + v1*(x1^alpha / (1+x1^alpha)) + d12 *(z12*(x2-x1))
    dz[2] = u2 - s2*x2 + v2*(x2^alpha / (1+x2^alpha)) + d21 *(z21*(x1-x2))
	dz
end

# parameters of the model
parms = (
    u1 = 0.2, u2 = 0.25, 
    s1 = 0.9, s2 = 0.95,
    v1 = 2., v2 = 2.1,
    d12 = 0.5, d21 = 0.3,
    z12 = 0.6, z21 = 0.5,
    alpha = 5. )

z0 = zeros(2)

# bifucation problem:
prob = BifurcationProblem(pollution!, z0,  params, (@lens _.u1); record_from_solution = recordFromSolution)


### Getting an error. Reproducing example from tutorial
# function to record information from a solution
recordFromSolution(x, p) = (u1 = x[1], u2 = x[2])

function pp2!(dz, z, p, t = 0)
	@unpack p1, p2, p3, p4 = p
	u1, u2 = z
	dz[1] = p2 * u1 * (1 - u1) - u1 * u2 - p1 * (1 - exp(-p3 * u1))
	dz[2] =	-u2 + p4 * u1 * u2
	dz
end

# parameters of the model
par_pp2 = (p1 = 1., p2 = 3., p3 = 5., p4 = 3.)

# initial condition
z0 = zeros(2)

# bifurcation problem
prob = BifurcationProblem(pp2!, z0, par_pp2,
	# specify the continuation parameter
	(@lens _.p1), recordFromSolution = recordFromSolution)

# continuation options?
opts_br = ContinuationPar(pMin = 0.1, pMax = 1.0, dsmax = 0.01,
	# options to detect bifurcations
	detectBifurcation = 3, nInversion = 8, maxBisectionSteps = 25,
	# number of eigenvalues
	nev = 2,
	# maximum number of continuation steps
	maxSteps = 1000)

diagram = bifurcationdiagram(prob, PALC(),
	# very important parameter. It specifies the maximum amount of recursion
	# when computing the bifurcation diagram. It means we allow computing branches of branches of branches
	# at most in the present case.
	3,
	(args...) -> setproperties(opts_br; ds = -0.001, dsmax = 0.01, nInversion = 8, detectBifurcation = 3);
	# δp = -0.01,
	verbosity = 0, plot = false)

scene = plot(diagram; code = (), title="$(size(diagram)) branches", legend = false)
