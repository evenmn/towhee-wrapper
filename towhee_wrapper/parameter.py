class Parameter:
    def __init__(self, label, type, default, *subpars):
        self.label = label
        self.type = type
        self.default = default
        self.value = default
        self.subpars = subpars

    def __repr__(self):
        repr_sub = []
        for subpar in self.subpars:
            repr_sub.append(subpar.__repr__())
        if len(repr_sub) > 0:
            return f"{self.label}({', '.join(repr_sub)})"
        else:
            return self.label

    def __str__(self):
        if self.type == int:
            return str(int(self.value))
        elif self.type == float:
            return str(float(self.value))
        elif self.type == str:
            return str(self.value)
        elif self.type == bool:
            if self.value is True:
                return ".TRUE."
            else:
                return ".FALSE."
        elif self.type == list:
            return "    ".join(self.value)

    def set_value(self, value):
        self.value = value


parameters = []
parameters.append(Parameter("inputformat", str, "Towhee"))
random_luxlevel = Parameter("random_luxlevel", int, 3)
parameters.append(Parameter("random_number_generator", str, "DX-1597-2-7",
                            random_luxlevel))
parameters.append(Parameter("random_seed", int, 1302002))
parameters.append(Parameter("random_allow_restart", bool, True))
parameters.append(Parameter("ensemble", str, None))
parameters.append(Parameter("temperature", float, None))
parameters.append(Parameter("pressure", float, None))
parameters.append(Parameter("nmolty", int, None))
parameters.append(Parameter("nmolectyp", list, None))
parameters.append(Parameter("chempot", list, None))
parameters.append(Parameter("numboxes", int, None))
parameters.append(Parameter("stepstyle", str, 'cycles'))
parameters.append(Parameter("nstep", int, None))
parameters.append(Parameter("controlstyle", str, None))
parameters.append(Parameter("printfreq", int, 0))
parameters.append(Parameter("blocksize", int, 0))
parameters.append(Parameter("moviefreq", int, 0))
parameters.append(Parameter("backupfreq", int, 0))
parameters.append(Parameter("restartfreq", int, 0))
parameters.append(Parameter("runoutput", str, 'full'))
parameters.append(Parameter("pdb_output_freq", int, 0))
parameters.append(Parameter("loutdft", bool, False))
parameters.append(Parameter("loutlammps", bool, False))
parameters.append(Parameter("loutdlpoly", bool, False))
n_tmmc_min = Parameter("n_tmmc_min", int, 0)
weight_freq = Parameter("weight_freq", int, 0)
c_matrix_freq = Parameter("c_matrix_freq", int, 0)
run_name = Parameter("run_name", str, 'tmmc_runfile')
in_cfile = Parameter("in_cfile", str, '')
in_c_flag = Parameter("in_c_flag", bool, False, in_cfile)
parameters.append(Parameter("tmmc_flag", bool, False, n_tmmc_min, weight_freq,
                            c_matrix_freq, run_name, in_c_flag))
hist_label = Parameter("hist_label", int, None)
hist_suffix = Parameter("hist_suffix", str, None)
hist_nequil = Parameter("hist_nequil", int, 0)
histcalcfreq = Parameter("histcalcfreq", int, None)
histdumpfreq = Parameter("histdumpfreq", int, None)
parameters.append(Parameter("louthist", bool, False, hist_label, hist_suffix,
                            hist_nequil, histcalcfreq, histdumpfreq))
parameters.append(Parameter("pressure_virial_freq", int, 0))
parameters.append(Parameter("pressure_thermo_freq", int, 0))
parameters.append(Parameter("pressure_thermo_style", str, 'range'))
parameters.append(Parameter("pressure_thermo_range", list, [-0.01, +0.01]))
parameters.append(Parameter("trmaxdispfreq", int, 0))
parameters.append(Parameter("volmaxdispfreq", int, 0))
parameters.append(Parameter("chempotperstep", int, 0))
parameters.append(Parameter("loutchempotdata", bool, False))
parameters.append(Parameter("linit", bool, None))
parameters.append(Parameter("initboxtype", str, None))
helix_moltyp = Parameter("helix_moltyp", int, None)
helix_radius = Parameter("helix_radius", float, None)
helix_angle = Parameter("helix_angle", float, None)
helix_keytype = Parameter("helix_keytype", str, None)
helix_keyname = Parameter("helix_keyname", str, None)
helix_conlen = Parameter("helix_conlen", float, None)
helix_phase = Parameter("helix_phase", float, None)
parameters.append(Parameter("initstyle", str, None, helix_moltyp, helix_radius,
                            helix_angle, helix_angle, helix_keytype,
                            helix_keyname, helix_conlen, helix_phase))
parameters.append(Parameter("initlattice", str, None))
parameters.append(Parameter("initmol", int, None))
parameters.append(Parameter("inix, iniy, iniz", list, None))
parameters.append(Parameter("hmatrix", list, None))
parameters.append(Parameter("box_number_density", float, None))
pmvlpr = Parameter("pmvlpr", float, None)
rmvol = Parameter("rmvol", float, None)
tavol = Parameter("tavol", float, None)
parameters.append(Parameter("pmvol", float, 0, pmvlpr, rmvol, tavol))
pmcellpr = Parameter("pmcellpr", float, None)
pmcellpt = Parameter("pmcellpt", float, None)
rmcell = Parameter("rmcell", float, None)
tacell = Parameter("tacell", float, None)
parameters.append(Parameter("pmcell", float, 0, pmcellpr, pmcellpt, rmcell,
                            tacell))
pm2rbswmt = Parameter("pm2rbswmt", float, None)
pm2rbswpr = Parameter("pm2rbswpr", float, None)
parameters.append(Parameter("pm2boxcbswap", float, 0, pm2rbswmt, pm2rbswpr))
pm2cbswmt = Parameter("pm2cbswmt", float, None)
pm2cbswpr = Parameter("pm2cbswpr", float, None)
parameters.append(Parameter("pm2boxcbswap", float, 0, pm2cbswmt, pm2cbswpr))
pm2comswboxpair = Parameter("pm2comswboxpair", float, None)
pm2comswmolpair = Parameter("pm2comswmolpair", float, None)
parameters.append(Parameter("pm2boxcomswitch", float, 0, pm2comswboxpair,
                            pm2comswmolpair))
pmuvtcbmt = Parameter("pmuvtcbmt", float, None)
parameters.append(Parameter("pmuvtcbswap", float, 0, pmuvtcbmt))
pm1comswbox = Parameter("pm1comswbox", float, None)
pm1comswpair = Parameter("pm1comswpair", float, None)
parameters.append(Parameter("pm1boxcbswap", float, 0, pm1comswbox,
                            pm1comswpair))
pmavb1in = Parameter("pmavb1in", float, None)
pmavb1mt = Parameter("pmavb1mt", list, None)
pmavb1ct = Parameter("pmavb1ct", list, None)
avb1rad = Parameter("avb1rad", float, None)
parameters.append(Parameter("pmavb1", float, 0, pmavb1in, pmavb1mt, pmavb1ct,
                            avb1rad))
pmavb2in = Parameter("pmavb2in", float, None)
pmavb2mt = Parameter("pmavb2mt", list, None)
pmavb2ct = Parameter("pmavb2ct", list, None)
avb2rad = Parameter("avb2rad", float, None)
parameters.append(Parameter("pmavb2", float, 0, pmavb2in, pmavb2mt, pmavb2ct,
                            avb2rad))
pmavb3mt = Parameter("pmavb3mt", list, None)
pmavb3ct = Parameter("pmavb3ct", list, None)
avb3rad = Parameter("avb3rad", float, None)
parameters.append(Parameter("pmavb3", float, 0, pmavb3mt, pmavb3ct, avb3rad))
pmcbmt = Parameter("pmcbmt", list, None)
pmall = Parameter("pmall", list, None)
parameters.append(Parameter("pmcb", float, 0, pmcbmt, pmall))
pmbkmt = Parameter("pmbkmt", list, None)
parameters.append(Parameter("pmback", float, 0, pmbkmt))
pmcbsidemt = Parameter("pmcbsidemt", list, None)
parameters.append(Parameter("pmcbside", float, 0, pmcbsidemt))
pmpivmt = Parameter("pmpivmt", list, None)
parameters.append(Parameter("pmpivot", float, 0, pmpivmt))
pmcrmt = Parameter("pmcrmt", list, None)
parameters.append(Parameter("pmconrot", float, 0, pmcrmt))
pmcrbmt = Parameter("pmcrbmt", list, None)
parameters.append(Parameter("pmcrback", float, 0, pmpivmt))
pmplanebox = Parameter("pmplanebox", list, None)
planewidth = Parameter("planewidth", float, None)
parameters.append(Parameter("pmplane", float, 0, pmplanebox, planewidth))
pmrowbox = Parameter("pmrowbox", list, None)
rowwidth = Parameter("rowwidth", float, None)
parameters.append(Parameter("pmrow", float, 0, pmrowbox, rowwidth))
pmtamt = Parameter("pmtamt", list, None)
rmtraa = Parameter("rmtraa", float, None)
tatraa = Parameter("tatraa", float, None)
parameters.append(Parameter("pmtraat", float, 0, pmtamt, rmtraa, tatraa))
pmcomt = Parameter("pmcomt", list, None)
rmcomtra = Parameter("rmcomtra", float, None)
pmcomrot = Parameter("pmcomrot", float, None)
parameters.append(Parameter("pmcomposite", float, 0, pmcomt, rmcomtra,
                            pmcomrot))
pmtcmt = Parameter("pmtcmt", list, None)
rmtrac = Parameter("rmtrac", float, None)
tatrac = Parameter("tatrac", float, None)
parameters.append(Parameter("pmtracm", float, 0, pmtcmt, rmtrac, tatrac))
pmromt = Parameter("pmromt", list, None)
rmrot = Parameter("rmrot", float, None)
tarot = Parameter("tarot", float, None)
parameters.append(Parameter("pmrotate", float, 0, pmromt, rmrot, tarot))
parameters.append(Parameter("cbmc_analysis", str, 'normal'))
parameters.append(Parameter("cbmc_formulation", str,
                            'Martin and Frischknecht 2006'))
parameters.append(Parameter("cbmc_setting_style", str, None))
parameters.append(Parameter("cbmc_fit_strategy", str, 'equilibrium'))
mapmolty = Parameter("mapmolty", int, None)
cube = Parameter("cubex,cubey,cubez", int, None)
lcreatemap = Parameter("lcreatemap", bool, None, cube)
parameters.append(Parameter("cbmc_nb_one_generation", str, 'uniform', mapmolty,
                            lcreatemap))
parameters.append(Parameter("nch_nb_box", list, [10]))
parameters.append(Parameter("nch_nb", list, [10]))
parameters.append(Parameter("nch_pre_nb", list, [1]))
dihedral_peak_weight_style = Parameter("dihedral_peak_weight_style", str,
                                       'uniform')
dihedral_sdev_multiplier = Parameter("dihedral_sdev_multiplier", float, 1.0)
dihedral_ideal_fraction = Parameter("dihedral_ideal_fraction", float, 0.01)
sdevtor = Parameter("sdevtor", float, 20.0)
parameters.append(Parameter("cbmc_dihedral_generation", str, 'ideal',
                            dihedral_peak_weight_style,
                            dihedral_sdev_multiplier,
                            dihedral_ideal_fraction, sdevtor))
parameters.append(Parameter("nch_tor", list, [360]))
parameters.append(Parameter("nch_tor_connect", list, [360]))
bend_a_sdev_multiplier = Parameter("bend_a_sdev_multiplier", float, 1.0)
bend_b_sdev_multiplier = Parameter("bend_b_sdev_multiplier", float, 1.0)
bend_a_ideal_fraction = Parameter("bend_a_ideal_fraction", float, 0.01)
bend_b_ideal_fraction = Parameter("bend_b_ideal_fraction", float, 0.01)
sdevbena = Parameter("sdevbena", float, 5.0)
sdevbenb = Parameter("sdevbenb", float, 5.0)
parameters.append(Parameter("cbmc_bend_generation", str, 'ideal',
                            bend_a_sdev_multiplier, bend_b_sdev_multiplier,
                            bend_a_ideal_fraction, bend_b_ideal_fraction,
                            sdevbena, sdevbenb))
parameters.append(Parameter("nch_bend_a", list, [100]))
parameters.append(Parameter("nch_bend_b", list, [100]))
parameters.append(Parameter("max_bond_length", float, 3.0))

print(random_luxlevel)
print(parameters)
