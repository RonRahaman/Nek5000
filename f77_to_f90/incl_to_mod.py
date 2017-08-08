import os, subprocess, re

for infileName in os.listdir():

    if infileName.upper() == infileName and infileName != 'HEADER':

        outfileName = "global_{}.F90".format(infileName.lower())
        moduleName =  "GLOBAL_{}".format(infileName.upper())

        # Read file contents
        with open(infileName, 'r') as infile:
            inputLines = infile.readlines()

        # Prepend/append module declarations around file contents
        header = [
            '      module {0}\n'.format(moduleName),
            '      use GLOBAL_SIZE\n',
            '      implicit none\n',
        ]
        footer = [
            '      end module {0}\n'.format(moduleName)
        ]
        outputLines = header + inputLines + footer

        outputLines =  [ re.sub(
            r'\s*include (?:\'|")(?!HEADER)(\w+)(?:\'|")',
            r'      use GLOBAL_\g<1>',
            line
        ) for line in outputLines ]

        # Rename the file
        # subprocess.run(['git', 'mv', infileName, outfileName])

        # Replace the file contents
        with open(outfileName, 'w') as outfile:
            outfile.writelines(outputLines)


