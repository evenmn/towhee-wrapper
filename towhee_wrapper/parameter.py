class Parameter:
    def __init__(self, type, default, **subparameters):
        self.type = type
        self.default = default
        self.value = default
        self.subparameters = subparameters

    def __repr__(self):
        type_subpar = []
        for key, obj in self.subparameters.items():
            type_subpar.append(str(obj.type))
        if len(type_subpar) == 0:
            return str(self.type)
        else:
            return f"{self.type}({', '.join(type_subpar)})"

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

    def set(self, value):
        self.value = self.type(value)


parameters = {}
parameters["inputformat"] = Parameter(str, "Towhee")
parameters["random_number_generator"] = Parameter(str, "DX-1597-2-7",
                                                  random_luxlevel=Parameter(int, 3))
parameters["random_seed"] = Parameter(int, 1302002)
parameters["random_allow_restart"] = Parameter(bool, True)
parameters["ensemble"] = Parameter(str, None)
parameters["temperature"] = Parameter(float, None)
parameters["pressure"] = Parameter(float, None)
parameters["nmolty"] = Parameter(int, None)
parameters["nmolectyp"] = Parameter(list, None)
parameters["chempot"] = Parameter(list, None)
parameters["numboxes"] = Parameter(int, None)
parameters["stepstyle"] = Parameter(str, 'cycles')
parameters["nstep"] = Parameter(int, None)
parameters["controlstyle"] = Parameter(str, None)
parameters["printfreq"] = Parameter(int, 0)
parameters["blocksize"] = Parameter(int, 0)
parameters["moviefreq"] = Parameter(int, 0)
parameters["backupfreq"] = Parameter(int, 0)
parameters["restartfreq"] = Parameter(int, 0)
parameters["runoutput"] = Parameter(str, 'full')
parameters["pdb_output_freq"] = Parameter(int, 0)
parameters["loutdft"] = Parameter(bool, False)
parameters["loutlammps"] = Parameter(bool, False)
parameters["loutdlpoly"] = Parameter(bool, False)
parameters["tmmc_flag"] = Parameter(bool, False,
                                    n_tmmc_min=Parameter(int, 0),
                                    weight_freq=Parameter(int, 0),
                                    c_matrix_freq=Parameter(int, 0),
                                    run_name=Parameter(str, 'tmmc_runfile'),
                                    in_c_flag=Parameter(bool, False,
                                                        in_cfile=Parameter(str, '')))
parameters["louthist"] = Parameter(bool, False,
                                   hist_label=Parameter(int, None),
                                   hist_suffix=Parameter(str, None),
                                   hist_nequil=Parameter(int, 0),
                                   histcalcfreq=Parameter(int, None),
                                   histdumpfreq=Parameter(int, None))
parameters["pressure_virial_freq"] = Parameter(int, 0)
parameters["pressure_thermo_freq"] = Parameter(int, 0)
parameters["pressure_thermo_style"] = Parameter(str, 'range')
parameters["pressure_thermo_range"] = Parameter(list, [-0.01, +0.01])
parameters["trmaxdispfreq"] = Parameter(int, 0)
parameters["volmaxdispfreq"] = Parameter(int, 0)
parameters["chempotperstep"] = Parameter(int, 0)
parameters["loutchempotdata"] = Parameter(bool, False)
parameters["linit"] = Parameter(bool, None)
parameters["initboxtype"] = Parameter(str, None)
parameters["initstyle"] = Parameter(str, None,
                                    helix_moltyp=Parameter(int, None),
                                    helix_radius=Parameter(float, None),
                                    helix_angle=Parameter(float, None),
                                    helix_keytype=Parameter(str, None),
                                    helix_keyname=Parameter(str, None),
                                    helix_conlen=Parameter(float, None),
                                    helix_phase=Parameter(float, None))
parameters["initlattice"] = Parameter(str, None)
parameters["initmol"] = Parameter(int, None)
parameters["inix, iniy, iniz"] = Parameter(list, None)
parameters["hmatrix"] = Parameter(list, None)
parameters["box_number_density"] = Parameter(float, None)

# Probabilities
parameters["pmvol"] = Parameter(float, 0,
                                pmvlpr=Parameter(float, None),
                                rmvol=Parameter(float, None),
                                tavol=Parameter(float, None))
parameters["pmcell"] = Parameter(float, 0,
                                 pmcellpr=Parameter(float, None),
                                 pmcellpt=Parameter(float, None),
                                 rmcell=Parameter(float, None),
                                 tacell=Parameter(float, None))
parameters["pm2boxrbswap"] = Parameter(float, 0,
                                       pm2rbswmt=Parameter(float, None),
                                       pm2rbswpr=Parameter(float, None))
parameters["pm2boxcbswap"] = Parameter(float, 0,
                                       pm2cbswmt=Parameter(float, None),
                                       pm2cbswpr=Parameter(float, None))
parameters["pm2boxcomswitch"] = Parameter(float, 0,
                                          pm2comswboxpair=Parameter(float, None),
                                          pm2comswmolpair=Parameter(float, None))
parameters["pmuvtcbswap"] = Parameter(float, 0,
                                      pmuvtcbmt=Parameter(float, None))
parameters["pm1boxcbswap"] = Parameter(float, 0,
                                       pm1comswbox=Parameter(float, None),
                                       pm1comswpair=Parameter(float, None))
parameters["pmavb1"] = Parameter(float, 0,
                                 pmavb1in=Parameter(float, None),
                                 pmavb1mt=Parameter(list, None),
                                 pmavb1ct=Parameter(list, None),
                                 avb1rad=Parameter(float, None))
parameters["pmavb2"] = Parameter(float, 0,
                                 pmavb2in=Parameter(float, None),
                                 pmavb2mt=Parameter(list, None),
                                 pmavb2ct=Parameter(list, None),
                                 avb2rad=Parameter(float, None))
parameters["pmavb3"] = Parameter(float, 0,
                                 pmavb3mt=Parameter(list, None),
                                 pmavb3ct=Parameter(list, None),
                                 avb3rad=Parameter(float, None))
parameters["pmcb"] = Parameter(float, 0,
                               pmcbmt=Parameter(list, None),
                               pmall=Parameter(list, None))
parameters["pmback"] = Parameter(float, 0,
                                 pmbkmt=Parameter(list, None))
parameters["pmcbside"] = Parameter(float, 0,
                                   pmcbsidemt=Parameter(list, None))
parameters["pmpivot"] = Parameter(float, 0,
                                  pmpivmt=Parameter(list, None))
parameters["pmconrot"] = Parameter(float, 0,
                                   pmcrmt=Parameter(list, None))
parameters["pmcrback"] = Parameter(float, 0,
                                   pmcrbmt=Parameter(list, None))
parameters["pmplane"] = Parameter(float, 0,
                                  pmplanebox=Parameter(list, None),
                                  planewidth=Parameter(float, None))
parameters["pmrow"] = Parameter(float, 0,
                                pmrowbox=Parameter(list, None),
                                rowwidth=Parameter(float, None))
parameters["pmtraat"] = Parameter(float, 0,
                                  pmtamt=Parameter(list, None),
                                  rmtraa=Parameter(float, None),
                                  tatraa=Parameter(float, None))
parameters["pmcomposite"] = Parameter(float, 0,
                                      pmcomt=Parameter(list, None),
                                      rmcomtra=Parameter(float, None),
                                      pmcomrot=Parameter(float, None))
parameters["pmtracm"] = Parameter(float, 0,
                                  pmtcmt=Parameter(list, None),
                                  rmtrac=Parameter(float, None),
                                  tatrac=Parameter(float, None))
parameters["pmrotate"] = Parameter(float, 0,
                                   pmromt=Parameter(list, None),
                                   rmrot=Parameter(float, None),
                                   tarot=Parameter(float, None))
parameters["cbmc_analysis"] = Parameter(str, 'normal')
parameters["cbmc_formulation"] = Parameter(str, 'Martin and Frischknecht 2006')
parameters["cbmc_setting_style"] = Parameter(str, None)
parameters["cbmc_fit_strategy"] = Parameter(str, 'equilibrium')
parameters["cbmc_nb_one_generation"] = Parameter(str, 'uniform',
                                                 mapmolty=Parameter(int, None),
                                                 lcreatemap=Parameter(bool, None,
                                                                      cube=Parameter(int, None)))
parameters["nch_nb_box"] = Parameter(list, [10])
parameters["nch_nb"] = Parameter(list, [10])
parameters["nch_pre_nb"] = Parameter(list, [1])
parameters["cbmc_dihedral_generation"] = Parameter(str, 'ideal',
                                                   dihedral_peak_weight_style=Parameter(str, 'uniform'),
                                                   dihedral_sdev_multiplier=Parameter(float, 1.0),
                                                   dihedral_ideal_fraction=Parameter(float, 0.01),
                                                   sdevtor=Parameter(float, 20.0))
parameters["nch_tor"] = Parameter(list, [360])
parameters["nch_tor_connect"] = Parameter(list, [360])
parameters["cbmc_bend_generation"] = Parameter(str, 'ideal',
                                               bend_a_sdev_multiplier=Parameter(float, 1.0),
                                               bend_b_sdev_multiplier=Parameter(float, 1.0),
                                               bend_a_ideal_fraction=Parameter(float, 0.01),
                                               bend_b_ideal_fraction=Parameter(float, 0.01),
                                               sdevbena=Parameter(float, 5.0),
                                               sdevbenb=Parameter(float, 5.0))
parameters["nch_bend_a"] = Parameter(list, [100])
parameters["nch_bend_b"] = Parameter(list, [100])
parameters["max_bond_length"] = Parameter(float, 3.0)
