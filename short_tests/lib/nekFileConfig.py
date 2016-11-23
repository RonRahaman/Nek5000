import os, stat, re


def config_makenek(opts, infile, outfile):
    """ Configure a makenek file by redefining the variables therein

    Given a path to an input makenek file ('infile'), set the variables and corresponding values from the dictionary
    'opts'.  Output the resultant makenek to 'outfile'. If 'infile' and 'outfile' are the same file, then the infile
    will be modified in-place.  Any variables not listed in 'opts' will be copied from 'infile' to 'outfile' without
    modification.

    For example, this creates a new makenek file (makenek.new) where F77, CC and G are set to pgf77, pgcc, and -g,
    respectively:
    config_makenek({'F77':'pgf77', 'CC':'pgcc 'G':'-g'} '/home/rahaman/Nek5000/core/makenek', '/home/rahaman/makenek.new')

    Only the options already available in makenek may be set.  This function doesn't allow you to define any new
    variables in makenek. (TODO: Raise warning when attempting to define a new variable.)

    Args:
        opts ({variable : value, ...}): Set each "variable=value" in the
                                        resultant makenek
        infile (str): Path to the input makenek file
        outfile (str): Path to the resultant makenek file

    """
    with open(infile, 'r') as f:
        lines = f.readlines()

    for key, val in opts.iteritems():
        lines = [re.sub(r'^#*{0}=\"+.+?\"+'.format(key), r'{0}="{1}"'.format(key, val), l) for l in lines]

    lines = [re.sub(r'(^source\s+\$SOURCE_ROOT/makenek.inc)', r'\g<1> >compiler.out', l)
             for l in lines]

    lines = [re.sub(r'(.+)2>&1\s+\|\s*tee\s+compiler.out', r'\g<1>', l)
             for l in lines]

    with open(outfile, 'w') as f:
        f.writelines(lines)
    os.chmod(outfile,
             stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH |
             stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH |
             stat.S_IWUSR)


def config_maketools(infile, outfile, f77=None, cc=None, bigmem=None):
    """ Configure a maketools file by redefining the variables therein

    Given a path to an input maketools file ('infile'), redefine F77, CC, and BIGMEM to the values provided by 'f77',
    'cc', and 'bigmem'.  Output the resultant makenek to 'outfile'. If 'f77', 'cc', or 'bigmem' are empty or None,
    then the corresponding variables will be copied from 'infile' to 'outfile without modification. If infile and
    outfile are the same file, then the infile will be reformatted in-place.

    Args:
        infile (str): Path to the input maketools file
        outfile (str): Path to the resultant maketools file
        f77 (str): Value for F77 variable in maketools
        cc (str): Value for CC variable in maketools
        bigmem (str): Value for BIGMEM variable in maketools

    """
    with open(infile, 'r') as f:
        lines = f.readlines()

    if f77:
        lines = [re.sub(r'^F77=\"+.+?\"+', r'F77="{0}"'.format(f77), l)
                 for l in lines]
    if cc:
        lines = [re.sub(r'^CC=\"+.+?\"+', r'CC="{0}"'.format(cc), l)
                 for l in lines]
    if bigmem:
        lines = [re.sub(r'BIGMEM=\"+.+?\"+', r'BIGMEM="{0}"'.format(bigmem), l)
                 for l in lines]

    with open(outfile, 'w') as f:
        f.writelines(lines)
    os.chmod(outfile,
             stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH |
             stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH |
             stat.S_IWUSR)


def config_basics_inc(infile, outfile, nelm):
    """ Configure a basics.inc file by redefining nelm

    If 'infile' and 'outfile' are the same, then 'infile' will be reformatted in-place

    Args:
        infile (str): Path to the input basics.inc file
        outfile (str): Path to the resultant basics.inc file
        nelm (str|int): Number of elements

    """
    with open(infile, 'r') as f:
        lines = f.readlines()

    lines = [re.sub(r'(.*nelm *= *)[ 0-9]+(.*)', r'\g<1>{0}\g<2>'.format(nelm), l, flags=re.I)
             for l in lines]

    with open(outfile, 'w') as f:
        f.writelines(lines)


def config_size(params, infile, outfile):
    """ Configure a SIZE file by redefining the variables therein

    Given a path to an input SIZE file ('infile'), redefine each parameter:value pair from the dictionary 'opts'.
    Output the resultant SIZE file to 'outfile'.  Any parameters from 'infile' that aren't listed in 'opts' will
    be copied from infile to outfile without modification. If infile and outfile are the same file, then
    the infile will be reformatted in-place.

    Only the parameters already defined in infile may be set.  This function doesn't allow you to define any new
    parameters. (TODO: Raise warning when attempting to define a new parameter.)

    Args:
        params ({variable : value, ...}): Each 'variable' will be set to 'value' in the output SIZE file
        infile (str): Path to the input SIZE file
        outfile (str): Path to output SIZE file

    """
    with open(infile, 'r') as f:
        lines = f.readlines()

    # Substitute all the variables
    for key, value in params.iteritems():
        if value:
            lines = [
                re.sub(
                    r'(.*\bparameter\b.*\b{0} *= *)\S+?( *[),])'.format(key),
                    r'\g<1>{0}\g<2>'.format(value), l, flags=re.I)
                for l in lines]

    with open(outfile, 'w') as f:
        f.writelines(lines)


def config_parfile(opts, infile, outfile):
    """ Configure a .par file by redefining the options therein.

    Given a path to an input .par file ('infile'), set all the options in the dictionary 'opts' (see below for
    explanation of dictionary format).  If infile and outfile are the same file, then the .par file will be modified
    in-place.

    opts is interpreted as a nested dict of the form:
        {section: {optname: value, ...}, ...}
    where "optname = value" are set in [section].
    * If 'optname' is not set in infile, then it will be added to outfile.
    * If 'optname' is already set in infile, then it will be overridden in outfile.
    * If an option is listed in infile but is not listed in in 'opts', then it will be copied unmodified to outfile.

    Args:
        opts ({section: {optname : value, ...}, ...}): Set each "optname = value" in the corresponding "[section]"
        infile (str): Path to input parfile
        outfile (str): Path to output parfile

    """
    import ConfigParser

    parfile = ConfigParser.SafeConfigParser()
    parfile.read(infile)

    for section, name_vals in opts.iteritems():
        for name, val in name_vals.iteritems():
            parfile.set(section, name, val)

    with open(outfile, 'w') as f:
        parfile.write(f)
