import os
import shutil
from parameter import parameters


class Towhee:
    def __init__(self, ensemble, temperature, nstep):

        self.parameters = parameters
        self.parameters["ensemble"].set(ensemble)
        self.parameters["temperature"].set(temperature)
        self.parameters["nstep"].set(nstep)

        self.boxes = []
        self.wd = ""

    # import
    from file_handling import read, write_input, write_parallel

    def set(self, keyword, value):
        self.parameters[keyword].set(value)

    def set_working_directory(self, wd, overwrite=False):
        self.wd = wd
        if overwrite:
            self.wd = wd
            try:
                os.makedirs(wd)
            except FileExistsError:
                pass
        else:
            ext = 0
            repeat = True
            while repeat:
                try:
                    os.makedirs(self.wd)
                    repeat = False
                except FileExistsError:
                    ext += 1
                    self.wd = wd + f"_{ext}"
        self.wd += "/"

    def copy_to_wd(self, *filename):
        """Copy one or several files to working directory.

        :param filename: filename or list of filenames to copy
        :type filename: str or list of str
        """

        for file in filename:
            head, tail = os.path.split(file)
            shutil.copyfile(file, self.wd + tail)

    def run(self, num_procs, towhee_exec='towhee'):
        """Run
        """
        self.write()
        self.write_parallel()


if __name__ == "__main__":
    towhee = Towhee('nvt', 300.0, 100)
    towhee.set("random_luxlevel", 2)
    towhee.write_input()
