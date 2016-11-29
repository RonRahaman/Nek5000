import os
import sys
from subprocess import call, check_call, Popen, PIPE, STDOUT
from lib.nekFileConfig import config_makenek, config_maketools, config_basics_inc

def build_tools(tools_root, tools_bin, f77='gfortran', cc='gcc', bigmem='false',
                targets=('clean', 'all'), verbose=False):
    """ Compile the Nek5000 tools in a given source directory

    Given a path to the top-level tools source directory ('tools_root'), compile the makefile targets specified by a
    list/tuple in 'targets', and output the executables in 'tools_bin'.  Targets are compiled in the order listed (
    so, for example, you can run 'make clean' by listing 'clean' first).  The values 'f77', 'cc', and 'bigmem' are
    used for F77, CC, and BIGMEM, respectively.

    For example, this command will compile genmap and genbox using the PGI compilers:
        build_tools('/home/rahaman/Nek5000/tools', '/home/rahaman/Nek5000/bin',
                    f77='pgf77', cc='pgcc', targets=('clean', 'genbox', 'genmap'))

    Args:
        tools_root (str): Path to top-level tools/ source directory
        tools_bin (str): Path to desired bin/ directory for resultant executables
        f77 (str): Fortran 77 compiler to use
        cc (str): C compiler to use
        bigmem (str): Option for BIGMEM variable in maketools (typically, the string 'true' or 'false')
        targets (list|tuple): A list of makefile targets to build.  Targets will be built in the order listed.
        verbose (bool): Specify True for verbose compilation

    """

    print('Compiling tools... ')
    print('    Using source directory "{0}"'.format(tools_root))
    print('    Using output directory "{0}"'.format(tools_bin))
    print('    Using F77 "{0}"'.format(f77))
    print('    Using CC "{0}"'.format(cc))

    maketools_in  = os.path.join(tools_root, 'maketools')
    maketools_out = os.path.join(tools_root, 'maketools.tests')
    maketools_log = os.path.join(tools_root, 'maketools.out')

    try:

        config_maketools(
            infile  = maketools_in,
            outfile = maketools_out,
            f77     = f77,
            cc      = cc,
            bigmem  = bigmem
        )

        config_basics_inc(
            infile  = os.path.join(tools_root, 'prenek', 'basics.inc'),
            outfile = os.path.join(tools_root, 'prenek', 'basics.inc'),
            nelm    = '10 000'
        )

        with open(maketools_log, 'w') as f:
            for t in targets:
                if verbose:
                    proc = Popen(
                        [maketools_out, t, tools_bin],
                        cwd=tools_root,
                        stderr=STDOUT,
                        stdout=PIPE
                    )
                    for line in proc.stdout:
                        sys.stdout.write(line)
                        f.write(line)
                else:
                    check_call(
                        [maketools_out, t, tools_bin],
                        stderr=STDOUT,
                        stdout=f,
                        cwd=tools_root
                    )
    except:
        print('Could not compile tools! Check "{0}" for details.'.format(maketools_log))
        raise
    else:
        print('Successfully compiled tools!')

def build_nek(source_root, usr_file, cwd=None, opts=None, verbose=False):
    """ Compile Nek5000 with the given options

    Given a path to the top-level source directory ('tools_root'), compile the makefile targets specified by a
    list/tuple in 'targets', and output the executables in 'tools_bin'.  Targets are compiled in the order listed (
    so, for example, you can run 'make clean' by listing 'clean' first).  The values 'f77', 'cc', and 'bigmem' are
    used for F77, CC, and BIGMEM, respectively.

    For example, this command will compile genmap and genbox using the PGI compilers:
        build_tools('/home/rahaman/Nek5000/tools', '/home/rahaman/Nek5000/bin',
                    f77='pgf77', cc='pgcc', targets=('clean', 'genbox', 'genmap'))

    Args:
        tools_root (str): Path to top-level tools/ source directory
        tools_bin (str): Path to desired bin/ directory for resultant executables
        f77 (str): Fortran 77 compiler to use
        cc (str): C compiler to use
        bigmem (str): Option for BIGMEM variable in maketools (typically, the string 'true' or 'false')
        targets (list|tuple): A list of makefile targets to build.  Targets will be built in the order listed.
        verbose (bool): Specify True for verbose compilation

    """

    if not opts:
        _opts = {}
    else:
        _opts = opts.copy()
    _opts.update(SOURCE_ROOT=source_root)

    print('Compiling nek5000...')
    print('    Using source directory "{0}"'.format(source_root))
    print('    Using working directory "{0}"'.format(cwd))
    print('    Using .usr file "{0}"'.format(usr_file))
    for key, val in _opts.iteritems():
        print('    Using {0}="{1}"'.format(key, val))

    makenek_in  = os.path.join(source_root, 'core', 'makenek')
    makenek_out = os.path.join(source_root, 'core', 'makenek.tests')
    logfile     = os.path.join(cwd, 'compiler.out')
    try:
        config_makenek(
            opts=_opts,
            infile=makenek_in,
            outfile=makenek_out
        )

        call([makenek_out, 'clean'], cwd=cwd)
        if verbose:
            with open(logfile, 'w') as f:
                proc = Popen([makenek_out, usr_file], cwd=cwd, stderr=STDOUT, stdout=PIPE)
                for line in proc.stdout:
                    sys.stdout.write(line)
                    f.write(line)
        else:
            with open(logfile, 'w') as f:
                call([makenek_out, usr_file], cwd=cwd, stdout=f)

    except:
        print('Could not compile nek5000!')
        raise
    else:
        print('Successfully compiled nek5000!')


