import os

for infileName in os.listdir():

    if infileName.upper() == infileName:
        moduleName =  infileName.lower()
        outfileName = "global_{}.F90".format(moduleName)
        #with open(infileName, '+') as infile, open(outfileName, 'w') as outFile:
        with open(infileName, 'r') as infile:
            inputLines = infile.readlines()
            outputLines = ['      module {0}\n'.format(moduleName)] \
                          + inputLines \
                          + ['      end module {0}\n'.format(moduleName)]
        with open(infileName, 'w') as outfile:
            outfile.writelines(outputLines)


