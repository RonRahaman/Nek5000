import os, subprocess

for infileName in os.listdir():

    if infileName.upper() == infileName and infileName != 'HEADER':

        moduleName =  infileName.lower()
        outfileName = "global_{}.F90".format(moduleName)

        # Read file contents
        with open(infileName, 'r') as infile:
            inputLines = infile.readlines()

        # Prepend/append module declarations around file contents
        header = [
            '      module {0}\n'.format(moduleName),
            '      use global_size\n',
            '      implicit none\n',
        ]
        footer = [
            '      end module {0}\n'.format(moduleName)
        ]
        outputLines = header + inputLines + footer

        # Rename the file
        subprocess.run(['git', 'mv', infileName, outfileName])

        # Replace the file contents
        with open(outfileName, 'w') as outfile:
            outfile.writelines(outputLines)


