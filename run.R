

# load the GLU package
library('GLU')

options(warn=2)


# parse arguments - directory is required, if filename is not specified then all files in directory are processed
opt = parseArgs()


# run GLU using command line arguments
runGLU(opt)


