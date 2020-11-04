import os


def read(filename='towhee_input'):
    # from __init__ import Towhee

    parameters = {}
    with open(filename, 'r') as f:
        for line in f:
            if line[0].islower():
                # keyword line
                if 'keyword' in locals():
                    parameters[keyword] = parameter
                parameter = ""
                keyword = line.split()[0]
                print(keyword)
            else:
                # parameter line
                parameter += line
                # print(parameter)
    print(parameters)


def write(self, filename='towhee_input'):
    """Write Towhee input script
    """
    filepath = os.path.join(self.wd, filename)

    with open(filepath, 'w') as f:
        for keyword in self.keywords:
            if self.parameter != self.parameters:
                f.write(keyword + "\n")
                f.write(value + "\n")


        f.write("\n")


def write_parallel(self, num_procs, filename="towhee_parallel"):
    """Write Towhee parallel script
    """
    filepath = os.path.join(self.wd, filename)

    with open(filepath, 'w') as f:
        f.write("\n")


if __name__ == "__main__":
    towhee = read("../data/towhee_input")
