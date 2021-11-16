# Income Explorer
This package contains the software for running the IncomeExplorer Application,
which enables the graphically interactive calculation and display of income components and 
effective marginal tax rate for user specified household parameters, with fixed
tax/welfare settings.

The underlying calculation is done by the function `emtr` in the file emtr.R,
which may be useful outside of the GUI.

## Tests
There is a suite of unit-tests (maybe these are better described end-to-end or
regression tests, as the unit being tested is quite large).  The tests are in 
tests/test_emtr.R and can be run from the base directory by (in Rstudio - there 
must be a way of doing this outside of Rstudio) ...

	library(testthat)
	test_dir("tests")

The output should look something like this:

    > test_dir("tests")
    √ |  OK F W S | Context
    √ | 162       | emtr [1.6 s]                                                                             
    √ |  46       | BestStart_IETC [4.2 s]                                                                   
    √ |  19       | IWTC_income-test [1.1 s]                                                                 
    √ |  10       | IWTC_phase-in [1.2 s]                                                                    
    √ |  11       | MFTC [0.9 s]                                                                             
    √ |   6       | WfF_AbatementOrder [0.9 s]                                                               
    
    == Results =================================================================
    Duration: 10.0 s
    
    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 254 ]
			
Pull requests should not be submitted if there are failing tests.
