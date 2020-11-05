def set_inputformat(self, format='Towhee'):
    """Set input format

    'Towhee' : (default) read the input parameters following the format for
               Towhee. This format is described in this file.
    'LAMMPS' : read the input parameters from the lammps_input and
               lammps_data files. Outputs files suitable for use with Towhee.
    """
    supported = ['Towhee', 'LAMMPS']
    if format in supported:
        self.parameters["inputformat"].set(format)
    else:
        raise NotImplementedError(f"Format {format} is not supported")


def set_rng(self, rng='DX-1597-2-7', luxlevel=3):
    """Set random number generator (RNG)

    'DX-1597-2-7' : (default) use the DX-1597-2-7 pseudorandom number
                    generator described in Deng 2005 with a period of
                    approximately 1014903.
    'KISS99' : use a version of the Keep it Simple Stupid pseudorandom
               number generator originally posted to an internet newsgroup
               by G. Marsaglia. This has a period of approximately 109.
    'MRG32k3a' : use the MRG32k3a pseudorandom number generator described
                 in l'Ecuyer 1999 with a period of approximately 1057.
    'RANLUX' : use the RANLUX psuedorandom number generator as described by
               Lüscher 1994 and implemented by James 1994 with a period of
               approximately 10171.

    When using this setting the following optional parameter is available
        random_luxlevel (integer) optional parameter
            An integer value that defines the luxury level used by the
            pseudorandom number generator. See James 1994 for more
            information about the luxury level.
            0: Does not discard any pseudorandom numbers from the sequence.
            1: Periodically discard 24 pseudorandom numbers from the sequence.
            2: Periodically discard 73 pseudorandom numbers from the sequence.
            3: (default) Periodically discard 199 pseudorandom numbers from the sequence.
            4: Periodically discard 365 pseudorandom numbers from the sequence.
    """
    supported = ['DX-1597-2-7', 'KISS99', 'MRG32k3a', 'RANLUX']
    if rng in supported:
        self.parameters.set(rng)
    else:
        raise NotImplementedError(
            f"Random number generator {rng} is not implemented")


def set_seed(self, seed=1302002):
    """Set seed to be used to initialize pseudo random number generator

    (default: 1302002) The 32 bit integer seed used to initialize the
    psuedorandom number generator (unless performing a restart when
    random_allow_restart is .TRUE.). Must be positive.
    """
    self.parameters["random_seed"].set(seed)


def allow_restart(self, restart=True):
    """Restart pseudo random number generator

    True: (default) restart the pseudorandom number generator from the
    sequence of integers in towhee_initial (if possible). This is done
    when the simulation is restarting from a towhee_initial file that was
    generated using Version 5.0.0 or later. If this is not possible then
    Towhee will initialize the pseudorandom number generator using the
    random_seed.
    False: initialize the pseudorandom number generator using the random_seed.
    """
    self.parameters["random_allow_restart"].set(restart)


def set_pressure(self, pressure):
    """Set external pressure, given in kPa. Only relevant if ensemble
    is set to 'npt'.
    """
    if self.parameters["ensemble"].value is not 'npt':
        raise UserWarning("Pressure will not be held constant in this ensemble")
    self.parameters["pressure"].set(pressure)


def set_chempot(self, chempot):
    """Set chemical potential, given in K. Only relevant if ensemble
    is set to 'uvt'.
    """
    if self.parameters["ensemble"].value is not 'uvt':
        raise UserWarning("Chemical potential will not be held constant in this ensemble")
    self.parameters["chempot"].set(chempot)


def set_maxmol(self, maxmol):
    """Set maximum number of molecules in the system. By default,
    this is set to the initial number of molecules.
    """
    self.parameters["nmolectyp"].set(maxmol)


def add_box(self, initboxtype):
    """Add box to the system
    """
    self.boxes.append(initboxtype)


def set_stepstyle(self, stepstyle='cycles'):
    """Set step style

    'cycles': (default) run a Monte Carlo simulation for nstep Monte Carlo
    cycles. A cycle is equal to N Monte Carlo moves, where N is the total
    possible number of molecules in this simulation (the sum of the
    nmolectyp values).
    'moves': run a Monte Carlo simulation for nstep Monte Carlo moves.
    """
    supported = ['cycles', 'moves']
    if stepstyle in supported:
        self.stepstyle = stepstyle
    else:
        raise NotImplementedError(
            f"Step style {stepstyle} is not implemented")


def set_nstep(self, nstep=10):
    """The number of Monte Carlo steps to perform where each step is either
    a full Monte Carlo cycle (if stepstyle is 'cycles') or a single move
    (if stepstyle is 'moves')
    """
    self.nstep = int(nstep)


def set_controlstyle(self, controlstyle='manual'):
    """Note that starting with version 8.0.0 this option merely changes the
    default values of certain towhee_input parameters, but they may still
    be reassigned manually.
    'equilibration' : designed for use when you are first setting up a
    simulation and still need to rapidly equilibrate all of the maximum
    update values. This option changes the default value of the following
    parameters based upon the value of nstep.
    'production' : designed for use during production runs. This option
    changes the default value of the following parameters based upon the
    value of nstep.
    'manual' : (default) the default values are not changed.
    """
    supported = ['equilibration', 'production', 'manual']
    if controlstyle in supported:
        self.constrolstyle = controlstyle
    else:
        raise NotImplementedError(
            f"Control style {controlstyle} is not implemented")


def set_printfreq(self, printfreq=0):
    """(default: 0, also see controlstyle) The step frequency for outputting
    information about the system to standard output. The information is the
    number of Monte Carlo steps performed thus far during the run, the total
    energy in each box, the x-box length of each box, the pressure of each
    box, and the number of molecules of each type in each box. If
    printfreq = 0 this information is not output.
    """
    self.printfreq = int(printfreq)


def set_blocksize(self, blocksize=0):
    """(default: 0, also see controlstyle) The size of the blocks for
    computing block averages. It is best if blocksize divides cleanly into
    nstep. The quantities that are averaged (in each simulation box) are the
    specific density, the pressure, all of the energy terms, the chemical
    potential of each molecule type, number density of each molecule type,
    and the mole fractions. If blocksize is 0 then no block output takes
    place.
    """
    self.blocksize = int(blocksize)


def set_moviefreq(self, moviefreq=0):
    """(default: 0, also see controlstyle) The step frequency for
    outputting the system information to the towhee_movie file. This file
    can be analyzed after the run using the analyse_movie utility routine to
    compute a variety of distribution functions. This file gets quite large
    if you output frequently so be careful if you have a limited amount of
    hard disk space available. If moviefreq is 0 this file is not written.
    """
    self.moviefreq = int(moviefreq)


def set_backupfreq(self, backupfreq=0):
    """(default: 0, also see controlstyle) The step frequency for writing
    a file named towhee_backup that is suitable for use as a restart file.
    It overwrites the previous version of towhee_backup each time so it does
    not take up much space. For more information about restart files see the
    Towhee restart manual. If backupfreq is 0 this file is not written.
    """
    self.backupfreq = int(backupfreq)


def set_restartfreq(self, restartfreq=0):
    """(default: 0) The step frequency for writing a file named
    towhee_restart_NNN that is suitable for use as a restart file, where NNN
    is the step number when this file is written. This file is identical to
    towhee_backup (see backupfreq), except that it is not overwritten by
    subsequent restart files, and therefore allows for restarts from
    multiple points along a run. For more information about restart files
    see the Towhee restart manual. If restartfreq is 0 this file is not
    written.
    """
    self.restartfreq = int(restartfreq)


def set_runoutput(self, runoutput='full'):
    """'full' : (default) output information about the individual blocks of
    the block averages and information about the maximum displacement
    updates.
    'blocks' : output information about the individual blocks of the block
    averages but not about the maximum displacement updates.
    'updates' : does not output information about the individual blocks of
    the block averages but does output information about the maximum
    displacement updates.
    'none' : does not output information about the individual blocks of the
    block averages or information about the maximum displacement updates.
    """
    supported = ['full', 'blocks', 'updates', 'none']
    if runoutput in supported:
        self.runoutput = runoutput
    else:
        raise NotImplementedError(
            f"Output style {runoutput} is not supported")


def set_pdb_output_freq(self, pdb_output_freq=0):
    """(default: 0) The step frequency for outputting a snapshot of the
    simulation to a pdb file named box_xx_step_yyyyyyyyyyyyyy.pdb where xx
    is the box number converted into a 2 character string and yyyyyyyyyyyyyy
    is the step number converted into a 14 character string. No pdb files
    are output if pdb_output_freq is 0.
    """
    self.pdb_output_freq = int(pdb_output_freq)


def output_dft(self, loutdft=False):
    """False: (default) do not output dft files.
    True: output files for use with the Tramonto classical density
    functional theory code. This outputs dft_surfaces.dat and
    dft_decode.dat. See the Tramonto software link for information
    about these files.
    """
    self.loutdft = bool(loutdft)


def output_lammps(self, loutlammps=False):
    """False: (default) do not output LAMMPS files.
    True: output files for use with the LAMMPS massively parallel molecular
    dynamics code. This outputs lammps_input and lammps_data# where the
    number is each of the simulation box numbers. See the LAMMPS software
    link for more information on how to read in these files.
    """
    self.loutlammps = bool(loutlammps)


def output_dlpoly(self, loutdlpoly=False):
    """False (default) do not output DL_POLY files.
    True: output files for use with the DL_POLY molecular dynamics code.
    This outputs CONFIG# and FIELD# where the number is each of the
    simulation box numbers. See the DL_POLY software link for more
    information about these files.
    """
    self.loutdlpoly = bool(loutdlpoly)


def set_tmmc_flag(self, tmmc_flag=False, n_tmmc_min=0, weight_freq=0,
                  c_matrix_freq=0, run_name='tmmc_runfile',
                  in_c_flag=False, in_cfile=""):
    """optional parameter, only if ensemble is 'uvt'
    False: (default) do not perform Transition-Matrix Monte Carlo (TMMC).
    True: perform Transition-Matrix Monte Carlo (TMMC) on a single component
    system in the grand canonical ensemble. This requires other parameters
    to be specified below.
    n_tmmc_min (integer) optional parameter, only if tmmc_flag is True
        (default: 0) Minimum number of molecules allowed in the simulation
        box. Specifying a non-zero value is useful when sampling specific
        molecule number ranges. Note that the corresponding n_tmmc_max is
        automatically set to the total number of molecules in the simulation.
    weight_freq (integer) optional parameter, only if tmmc_flag is .TRUE.
        (default: 0) The step frequency for updating the biasing function
        for Transition-Matrix Monte Carlo. Updates are not performed if
        weight_freq is 0.
    c_matrix_freq (integer) optional parameter, only if tmmc_flag is True
        (default: 0) The step frequency for writing to file the collection
        matrix for TMMC. A new output file containing the step number is
        created. These files are not output if c_matrix_freq is 0.
    run_name (character) optional parameter, only if tmmc_flag is True
        (default: 'tmmc_runfile') File prefix for TMMC output. The current
        estimate of the natural logarithm of the particle number probability
        distribution is written to a file "run_name.tmmc_weights.dat" every
        time the biasing function is updated. Collection matrix info is
        written to a file "run_name.c.stepnumber.dat". Accumulated semigrand
        potential energies are written to a file "run_name.vsg.stepnumber".
        This information is useful if one is interested in the average
        potential energy as a function density.
    in_c_flag (logical) optional parameter, only if tmmc_flag is True
        .FALSE.: (default) start with the default collection matrix.
        .TRUE.: read an initial collection matrix from a file and use that
        to initiate the TMMC simulation. This option is used to continue a
        previous TMMC simulation. Future versions could also use this to
        read in either a collection matrix or biasing function.
    in_cfile (character) only if in_c_flag is True
        Text prepended to the collection matrix input file.
    """
    if self.ensemble is not 'uvt':
        raise UserWarning("TMMC flag will be ignored in this ensemble")
    self.tmmc_flag = bool(tmmc_flag)
    self.n_tmmc_min = int(n_tmmc_min)
    self.weight_freq = int(weight_freq)
    self.c_matrix_freq = int(c_matrix_freq)
    self.run_name = str(run_name)
    self.c_flag = bool(c_flag)
    self.cfile = str(cfile)


def output_hist(self, louthist=False, hist_label=0, hist_suffix="",
                hist_nequil=0, histcalcfreq=0, histdumpfreq=0):
    """optional parameter, only if ensemble is 'uvt'
    False: (default) histogram reweighting file are not output.
    True: output files used for histogram reweighting. This value
    requires the following additional parameters
    hist_label (integer) only if louthist is True
        An integer that is turned into a character string that creates
        the X portion of the named towhee_hisXY.dat file that is used
        to output information for histogram reweighting.
    hist_suffix (character*1) only if louthist is True
        A single character that creates the Y portion of the named
        towhee_hisXY.dat file that is used to output information for
        histogram reweighting.
    hist_nequil (integer) only if louthist is True
        The number of steps (cycles or moves) that are disregarded for
        the purposes of outputting histogram information. Set to 0 if
        you wish to use all of the steps for computing the histogram,
        and set to a positive number in order to discard those initial
        steps from the histogram.
    histcalcfreq (integer) only if louthist is True
        The step frequency for computing the information needed for
        histogram reweighting.
    histdumpfreq (integer) only if louthist is True
        The step frequency for outputting the information needed for
        histogram reweighting to the various towhee_histogram files.
        The ratio of histdumpfreq/histcalcfreq must be less than
        NDUMPHIST.
    """
    if self.ensemble is not 'uvt':
        raise UserWarning("TMMC flag will be ignored in this ensemble")
    self.louthist = bool(louthist)
    self.hist_label = int(hist_label)
    self.hist_suffix = int(hist_suffix)
    self.hist_nequil = int(hist_nequil)
    self.histcalcfreq = int(histcalcfreq)
    self.histdumpfreq = int(histdumpfreq)


def set_pressure_virial_freq(self, pressure_virial_freq=0):
    """optional parameter
    (default: 0, also see controlstyle) The step frequency for computing
    the pressure using a deterministic algorithm that is based upon the
    interaction of pairs of molecules in each simulation box. This is
    performed using the molecular version of the pressure virial (for
    continuous potentials) or the radial pressure (for discontinuous
    potentials). See the Pressure Manual for more information about the
    pressure calculation methods implemented into Towhee. Computing the
    pressure using these algorithms is a relatively expensive task
    (compared to the cost of a Monte Carlo move), especially for large
    systems. This calculation is not performed if pressure_virial_freq
    is 0.
    """
    self.pressure_virial_freq = int(pressure_virial_freq)


def set_pressure_thermo_freq(self, pressure_thermo_freq=0):
    """optional parameter
    (default: 0) The step frequency for computing the pressure by
    sampling the average energy change/volume change ratio to compute
    the "thermodynamic" pressure each simulation box. This calculation
    is not yet functional (as of version 8.0.0). This calculation is
    not performed if pressure_thermo_freq is 0.
    """
    self.pressure_thermo_freq = int(pressure_thermo_freq)


def set_pressure_thermo_style(self, pressure_thermo_style='range'):
    """optional parameter
    The style of attempted volume change to use when computing the
    thermodynamic pressure via additional ghost volume changes.
        'move data': attempts volume changes in the same manner as
        performing a single box isobaric-isothermal volume move.
        'range': (default) attempts a ghost volume change of size Volume
        * Uniform[pressure_thermo_range(0), pressure_thermo_range(1)].
    """
    supported = ['move data', 'range']
    if pressure_thermo_style in supported:
        self.pressure_thermo_style = pressure_thermo_style
    else:
        raise NotImplementedError(f"Pressure-thermo style \
            {pressure_thermo_style} is not supported")


def set_pressure_thermo_range(self, low=-0.01, high=0.01):
    """optional parameter
    (default: -0.01d0 0.01d0) The range for the attempted ghost volume
    changes used when computing the thermodynamic pressure.
    """
    self.pressure_thermo_min = float(low)
    self.pressure_thermo_max = float(high)


def set_trmaxdispfreq(self, trmaxdispfreq=0):
    """optional parameter
    (default: 0, also see controlstyle) The step frequency for updating
    the maximum translational (atom and center-of-mass) and rotational
    displacements. They are periodically adjusted to achieve the target
    acceptance rates (see tatraa, tatrac, and tarot). When initially
    equilibrating a simulation it is a good idea to do this frequently
    (every step or every 10 steps) in order to quickly converge to
    useful values for the maximum displacements. Once the acceptance
    rates are near the target values then setting trmaxdispfreq to
    perform about 10 updates per simulation is probably sufficient.
    Updates to these quantities are not performed if trmaxdispfreq is 0.
    """
    self.trmaxdispfreq = int(trmaxdispfreq)


def set_volmaxdispfreq(self, volmaxdispfreq=0):
    """optional parameter
    (default: 0, also see controlstyle) The step frequency for updating
    the maximum volume displacements. They are periodically adjusted to
    achieve the target acceptance rate (see tavol). When initially
    equilibrating a simulation it is a good idea to do this frequently
    (every few steps) in order quickly converge to useful values for the
    maximum displacements. Once the acceptance rates are near their
    desired values it is suggested to set volmaxdisp to perform around
    10 updates during the simulation. Updates to the maximum volume
    displacements are not performed if volmaxdispfreq is set to 0.
    """
    self.volmaxdispfreq = int(volmaxdispfreq)


def set_chempotperstep(self, chempotperstep=0):
    """optional parameter
    (default: 0) The number of additional trial insertions to perform
    in each box for at the end of every Monte Carlo step (listed
    sequentially for each molecule type on a single line). This allows
    the measurement of chemical potential in ensembles that do not have
    an insertion and deletion move (such as canonical and
    isobaric-isothermal).
    """
    self.chempotperstep = int(chempotperstep)


def output_chempotdata(self, loutchempotdata=False):
    """optional parameter
    True: output chemical potential insertion data to a file named
    towhee_chempotdata every time an insertion is attempted.
    False: (default) chemical potential insertion data is not output to
    a separate file.
    """
    self.loutchempotdata = bool(loutchempotdata)


def initialize(self, linit):
    """True: start a new simulation and generate the positions of all
    of the atoms, assign initial box dimensions and maximum
    displacements.
    False: continue the simulation by reading in box lengths, maximum
    displacements, and coordinates from towhee_initial.
    """
    self.linit = bool(linit)


def set_initboxtype(self, initboxtype):
    """'dimensions' : the dimensions of the initial boxes are entered
    in order to construct the initial boxes.
    'number density' : the total number density of molecules in the
    initial boxes are entered in order to compute the initial sizes of
    the boxes. This option generates cubic boxes.
    'unit cell' : generates an initial structure by duplicating a unit
    cell. Reads information from the towhee_cell file and uses that to
    create an initial structure.
    """
    supported = ['dimensions', 'number density', 'unit cell']
    if initboxtype in supported:
        self.initboxtype = initboxtype
    else:
        raise NotImplementedError(f"Initial box type \
            {initboxtype} is not supported")
