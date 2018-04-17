#!/usr/bin/env python
# -*- coding: utf-8 -*-
""" kgen kernel classification. """
from __future__ import absolute_import, division, print_function

import os
import sys
import argparse
import shutil
import glob
import subprocess
import string
import math

current_module = sys.modules[__name__]
current_dir = os.path.dirname(os.path.abspath(__file__))

##############################################################
# script main
##############################################################

classified_dirname = "classified"

group_readme = """
%(by)s
"""

def error(msg):
    print("ERROR: %s"%msg)
    sys.exit(1)

# create kernel classification
def create():
    #parser = argparse.ArgumentParser(
    #    description='Create KGen kernel classification')
    #parser.add_argument('topdir')
    #parser.add_argument('--amend', action='store_true')
    #args = parser.parse_args(sys.argv[2:])

    topdir = os.path.realpath(os.path.join(current_dir, '..'))
    kerneldir = os.path.join(topdir, 'all')
    if not os.path.exists(kerneldir):
        error("Kernel directory, \"%s\", does not exists."%kerneldir)

    # iterate kernels 
    kernels = []
    for path in os.listdir(kerneldir):
        if os.path.isdir(os.path.join(kerneldir, path)) and not path.startswith("_"):
            kernels.append(Kernel(kerneldir, path))

    #kernels = [Kernel(args.topdir, path) for path in os.listdir(args.topdir)
    #    if os.path.isdir(os.path.join(args.topdir, path)) and
    #    not path.startswith("_")]
    if len(kernels) == 0:
        error("No kernels is found.")

    # collect classification definitions
    classfiers = []
    for obj in current_module.__dict__.values(): 
        if hasattr(obj, 'classifier_decorated'):
            classfiers.append(obj)
    if len(classfiers) == 0:
        error("No classification is defined.")

    # filter classification definitions

    # create classification top directory
    classified_dir = os.path.join(topdir, classified_dirname)
    if os.path.exists(os.path.join(classified_dir, classified_dirname)):
        error("Classification directory, \"%s\", already exists."%classified_dir)

    # iterate classification definitions
    try:
        for classfier in classfiers:
            classification_desc, kernel_groups = classfier(kernels)
            # create group directory and save meta data of group and kernels
            workdir = os.path.join(classified_dir, classfier.__name__)
            os.makedirs(workdir)
            with open(os.path.join(workdir, "README"), 'w') as f:
                f.write(group_readme%{"by": classification_desc})
            for group in kernel_groups:
                if len(group.kernels) > 0:
                    groupdir = os.path.join(workdir, group.name)
                    os.makedirs(groupdir)
                    for kernel in group.kernels:
                        link_name = os.path.join(groupdir, kernel.dirname)
                        os.symlink(kernel.kernelpath, link_name)

            # generate links of kernels
    except OSError as err:
        error(str(err))

# update kernel classification
def update():
    # check if there is no changed files

    # delete current classification

    # add new classification

    # git add and commit the changes
    pass

# delete kernel classification
def delete():
    #parser = argparse.ArgumentParser(
    #    description='Delete KGen kernel classification')
    #parser.add_argument('topdir')
    #parser.add_argument('--amend', action='store_true')
    #args = parser.parse_args(sys.argv[2:])

    topdir = os.path.realpath(os.path.join(current_dir, '..'))
    classified_dir = os.path.join(topdir, classified_dirname)
    if not os.path.exists(classified_dir):
        error("Classification directory, \"%s\", does not exists."%classified_dir)

    for root, dirs, files in os.walk(classified_dir, topdown=False):
        for name in files:
            os.remove(os.path.join(root, name))
        for name in dirs:
            try:
                os.rmdir(os.path.join(root, name))
            except:
                os.unlink(os.path.join(root, name))

    if os.path.exists(classified_dir):
        os.rmdir(classified_dir)

##############################################################
# Common routines for classification definitions
##############################################################

def classifier(func):
    func.classifier_decorated = True
    return func

class KernelGroup(object):
    def __init__(self, groupname, gropudesc, kernels):
        self.name = groupname # used for directory name
        self.desc = gropudesc # used in README
        self.kernels = kernels


class Kernel(object):

    compilers = ('', 'intel', 'pgi')
    systems = ('knl', 'knc', 'haswell', 'sandybridge')

    def __init__(self, toppath, kerneldirname):
        self.toppath = os.path.abspath(toppath)
        self.dirname = kerneldirname
        self.kernelpath = os.path.join(toppath, self.dirname)
        self.has_orig = False
        self.has_data = False
        self.has_final = False
        self.has_config = False
        self.has_state = False
        self.readme = None
        self.makefiles = []
        self.kernel_driver = None
        self.gitadded_datetime = None

        self.validated = self.validate()

    def validate(self):

        self.has_orig = os.path.isdir(os.path.join(self.kernelpath, 'orig'))
        self.has_data = os.path.isdir(os.path.join(self.kernelpath, 'data'))
        self.has_final = os.path.isdir(os.path.join(self.kernelpath, 'final'))
        self.has_config = os.path.isdir(os.path.join(self.kernelpath, 'config'))
        self.has_state = os.path.isdir(os.path.join(self.kernelpath, 'state'))

        self.readme = os.path.join(self.kernelpath, 'README') if \
            os.path.isfile(os.path.join(self.kernelpath, 'README')) else None
        self.readme = os.path.join(self.kernelpath, 'README.txt') if \
            self.readme is None and os.path.isfile(os.path.join(self.kernelpath, 'README.txt')) else None
        self.readme = os.path.join(self.kernelpath, 'README.md') if \
            self.readme is None and os.path.isfile(os.path.join(self.kernelpath, 'README.md')) else None

        for makefile in glob.glob(os.path.join(self.kernelpath, 'orig', '[m|M]akefile*')):
            attrs = {}
            attrs['path'] = makefile
            with open(makefile) as f:
                attrs['kgensignature'] = f.readline().upper().find('KGEN')
                for line in f:
                    items = line.split()
                    if len(items) > 0 and items[0].startswith("FC"):
                        attrs[items[0]] = items[2:]
                        
            self.makefiles.append(attrs)

        kernel_driver = os.path.join(self.kernelpath, 'orig', 'kernel_driver.f90')
        if 'kernel_driver.f90' in os.listdir(os.path.join(self.kernelpath, 'orig')):
            attrs = {}
            attrs['path'] = kernel_driver
            with open(kernel_driver) as f:
                for line in f:
                    comment = line[0] == "!"
                    poscolon = line.find(":")
                    if comment and poscolon > 0:
                        if line[1:poscolon].strip() == "Generated at":
                            attrs['creation_datetime'] = line[poscolon+1:].strip()
                        elif line[1:poscolon].strip() == "KGEN version":
                            attrs['kgen_version'] = line[poscolon+1:].strip()
                            break

            self.kernel_driver = attrs

        kernel_file = kernel_driver
        if 'kernel_driver.f90' not in os.listdir(os.path.join(self.kernelpath, 'orig')):
            for makefile in self.makefiles:
                kernel_file = makefile['path']
                break

        if os.path.isfile(kernel_file):
            git_command = ["git", "blame", kernel_file]
            head_command = ["head", "-n", "1"]
            tail_command = ["tail", "-n", "1"]

            p1 = subprocess.Popen(git_command, stdout=subprocess.PIPE)
            p2 = subprocess.Popen(tail_command, stdin=p1.stdout, stdout=subprocess.PIPE)
            out, err = p2.communicate()

            output = out.split()
            if len(output) > 4:
                term3 = output[3]
                term4 = output[4]
                if len(term3)>4 and term3[:4].isdigit():
                    self.gitadded_datetime = output[3:5]
                elif len(term4)>4 and term4[:4].isdigit():
                    self.gitadded_datetime = output[4:6]

                if not self.gitadded_datetime:
                    import pdb; pdb.set_trace()

        if self.has_orig and len(self.makefiles) > 0:
            return True
        return False

    @property
    def name(self):
        if self.has_data:
            for datafile in glob.glob(os.path.join(self.kernelpath, 'data/*')):
                items = os.path.basename(datafile).split(".")
                if len(items) > 1:
                    return items[0]

        return self.dirname

# by application


# by importance


# by similar filenames

##############################################################
# Classifiers 
##############################################################

########### classifier utilities #############################

def get_numberoffiles(kernel):
    ver = None
    num = 0
    if kernel.has_orig:
        ver = "orig"
    elif kernel.has_final:
        ver = "final"
    if ver:
        for f in os.listdir(os.path.join(kernel.kernelpath, ver)):
            root, ext = os.path.splitext(f)
            if ext == ".f90" or ext == ".F90":
                num += 1
    return num


@classifier
def by_alphabet(kernels):

    def grouping(kernels, ch, classfied):
        group = []
        for kernel in kernels:
            if kernel.name[0].lower() == ch:
                group.append(kernel)
                classfied[kernel] = True
        return group

    classification_desc = "Classified by the first character of the kernel name"
    classfied = dict((kernel, False) for kernel in kernels)
    kernel_groups = [KernelGroup(ch, "Kernels whose name starts with '%s'"%ch,
        grouping(kernels, ch, classfied)) for ch in string.ascii_lowercase]
    unclassified = [k for k, c in classfied.items() if not c]
    if len(unclassified) > 0:
        kernel_groups.append(KernelGroup("Unclassified", "Unclassified.", unclassified))

    return classification_desc, kernel_groups

@classifier
def by_numberoffiles(kernels):
    MAX_RANGES = 5
    def get_ranges(kernels):
        nfiles = [] 
        minfiles = sys.maxsize
        maxfiles = 0
        for kernel in kernels:
            num = get_numberoffiles(kernel)
            nfiles.append(num)
            minfiles = min(minfiles, num)
            maxfiles = max(maxfiles, num)

        ranges = []
        if len(nfiles) < MAX_RANGES:
            for nfile in nfiles:
                ranges.append((nfile, nfile))
        else:
            delta = float(maxfiles-minfiles)/MAX_RANGES
            begin = minfiles
            for i in range(MAX_RANGES-1):
                ranges.append((begin, int(math.floor(begin+delta))))
                begin += int(math.ceil(delta))
            ranges.append((begin, maxfiles))
        return ranges
                
    def grouping(kernels, r, classfied):
        group = []
        for kernel in kernels:
            num = get_numberoffiles(kernel)
            if num >= r[0] and num <= r[1]:
                group.append(kernel)
                classfied[kernel] = True
        return group

    classification_desc = "Classified by the number of kernel source files"
    ranges = get_ranges(kernels)
    classfied = dict((kernel, False) for kernel in kernels)
    kernel_groups = [KernelGroup("%d-%d"%r, "Kernels whose number of source files are between %d and %d"%r,
        grouping(kernels, r, classfied)) for r in ranges]
    unclassified = [k for k, c in classfied.items() if not c]
    if len(unclassified) > 0:
        kernel_groups.append(KernelGroup("Unclassified", "Unclassified.", unclassified))
    return classification_desc, kernel_groups

@classifier
def by_gitadded_year(kernels):
    def get_years(kernels):
        years = []
        for kernel in kernels:
            if kernel.gitadded_datetime:
                year = kernel.gitadded_datetime[0][:4]
                if year not in years:
                    years.append(year)
        return years
                
    def grouping(kernels, y, classfied):
        group = []
        for kernel in kernels:
            if kernel.gitadded_datetime:
                year = kernel.gitadded_datetime[0][:4]
                if year == y:
                    group.append(kernel)
                    classfied[kernel] = True
        return group

    classification_desc = "Classified by the first added year into git repository"
    years = get_years(kernels)
    classfied = dict((kernel, False) for kernel in kernels)
    kernel_groups = [KernelGroup(y, "Kernels whose git-added year is %s"%y,
        grouping(kernels, y, classfied)) for y in years]
    unclassified = [k for k, c in classfied.items() if not c]
    if len(unclassified) > 0:
        kernel_groups.append(KernelGroup("Unclassified", "Unclassified.", unclassified))
    return classification_desc, kernel_groups

@classifier
def by_makefile(kernels):

    def get_makefiles(kernels):
        makefiles = set()
        for kernel in kernels:
            for makefile in kernel.makefiles:
                makefiles.add(os.path.basename(makefile['path'])) 
                if "Makefile" in makefiles:
                    import pdb; pdb.set_trace()
        return tuple(makefiles)

    def grouping(kernels, gm, classfied):
        group = []
        for kernel in kernels:
            for km in kernel.makefiles:
                makefile = os.path.basename(km['path'])
                if makefile == gm:
                    group.append(kernel)
                    classfied[kernel] = True
                    break
        return group

    classification_desc = "Classified by the postfix to Makefile name"
    classfied = dict((kernel, False) for kernel in kernels)
    makefiles = get_makefiles(kernels)
    kernel_groups = [KernelGroup(m, "Kernels whose makefile is '%s'"%m,
        grouping(kernels, m, classfied)) for m in makefiles]
    unclassified = [k for k, c in classfied.items() if not c]
    if len(unclassified) > 0:
        kernel_groups.append(KernelGroup("Unclassified", "Unclassified.", unclassified))

    return classification_desc, kernel_groups

@classifier
def by_similarfilename(kernels):

    def get_groups(kernels):

        min_common_files = 5
        cutoff_distance = 0.1

        data = []
        for i in range(len(kernels)):
            k1 = kernels[i]
            for j in range(i+1,len(kernels)):
            #for j in range(0,len(kernels)):
                k2 = kernels[j]
                ncommon = 0
                if os.path.isdir(os.path.join(k1.kernelpath, 'orig')) and \
                    os.path.isdir(os.path.join(k2.kernelpath, 'orig')):
                    k1files = []
                    commonfiles = []
                    for f in os.listdir(os.path.join(k1.kernelpath, 'orig')):
                        if f.endswith("f") or f.endswith("F") or \
                            f.endswith("f90") or f.endswith("F90"):
                            k1files.append(os.path.basename(f))
                    for f in os.listdir(os.path.join(k2.kernelpath, 'orig')):
                        if os.path.basename(f) in k1files:
                            commonfiles.append(f)
                            ncommon += 1
                if ncommon > min_common_files:
                    #data.append((k1, k2, commonfiles, ncommon))
                    data.append((k1, k2, float(1)/(ncommon-min_common_files)))

        from dbscan import DBSCAN
        db = DBSCAN(eps=1 , min_points=4)
        db.cluster(data)
        for i in range(len(db.clusters)):
            cname, c = db.clusters[i]
            newc = []
            for j in range(len(c)):
                if c[j][2] < cutoff_distance:
                    newc.append(c[j])
            if len(newc) > 0:
                db.clusters[i] = (cname, newc) 
        return db.clusters

    def grouping(kernels, points, classfied):
        group = set()
        for k1, k2, dist in points:
            group.add(k1)
            group.add(k2)
            classfied[k1] = True
            classfied[k2] = True
        return group

    classification_desc = "Classified by similar filenames"
    classfied = dict((kernel, False) for kernel in kernels)
    groups = get_groups(kernels)
    kernel_groups = [KernelGroup(gname, gname,
        grouping(kernels, points, classfied)) for gname, points in groups]
    unclassified = [k for k, c in classfied.items() if not c]
    if len(unclassified) > 0:
        kernel_groups.append(KernelGroup("Unclassified", "Unclassified.", unclassified))

    return classification_desc, kernel_groups

@classifier
def by_kernelprefix(kernels):
    def get_prefixes(kernels):
        min_group_size = 2
        prefixes = {}
        for kernel in kernels:
            kname = os.path.basename(kernel.kernelpath)
            pos = kname.find("_")
            if pos > 0:
                try:
                    prefixes[kname[:pos]] += 1
                except:
                    prefixes[kname[:pos]] = 1
        output = [k for k, v in prefixes.items() if v >= min_group_size]
        return output

    def grouping(kernels, p, classfied):
        group = []
        for kernel in kernels:
            if os.path.basename(kernel.kernelpath).startswith(p):
                group.append(kernel)
                classfied[kernel] = True
        return group

    classification_desc = "Classified by the same prefix of the kernel directory"
    classfied = dict((kernel, False) for kernel in kernels)
    prefixes = get_prefixes(kernels)
    kernel_groups = [KernelGroup(p, "Kernels whose name prefix is '%s'"%p,
        grouping(kernels, p, classfied)) for p in prefixes]
    unclassified = [k for k, c in classfied.items() if not c]
    if len(unclassified) > 0:
        kernel_groups.append(KernelGroup("Unclassified", "Unclassified.", unclassified))

    return classification_desc, kernel_groups

    pass

##############################################################
# by the year when the kernel is first uploaded
##############################################################

##############################################################
# By the number of source files
##############################################################

##############################################################
# By the KGen version used for extraction 
##############################################################


##############################################################
# Execution starts here 
##############################################################

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
            description='KGen kernel classification',
            usage='''%(prog)s <command> [<args>]

The most commonly used commands are:
   create     create classification 
   update     update classification 
   delete     delete classification 
''')
    parser.add_argument('command', help='Subcommand to run')
    args = parser.parse_args(sys.argv[1:2])
    if not hasattr(current_module, args.command):
        print('Unrecognized command')
        parser.print_help()
        exit(1)
    getattr(current_module, args.command)()

