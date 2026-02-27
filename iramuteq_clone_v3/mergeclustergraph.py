# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import sys

#------------------------------------
# import des modules wx
#------------------------------------

#------------------------------------
# import des fichiers du projet
#------------------------------------
from functions import DoConf, read_chd, ReadProfileAsDico
from chemins import ConstructPathOut, ChdTxtPathOut, FFF, ffr, PathOut, StatTxtPathOut, simipath
from layout import SimiLayout
from textsimi import *
from analyse_merge import AnalyseMerge


class MergeClusterGraph :

    def __init__(self, ira, corpus, parametres) :
        self.ira = ira
        self.corpus = corpus
        self.parametres = parametres
        self.pathout = PathOut(parametres['ira'])
        self.pathout.basefiles(ChdTxtPathOut)
        self.corpus.make_ucecl_from_R(self.pathout['uce'])
        self.encoding = self.parametres['encoding']
        self.clnb = parametres['clnb']
        dictprofile = ReadProfileAsDico(self.pathout['PROFILE_OUT'], True, self.encoding)
        self.dograph(dictprofile)

    def dograph(self, dictprofile) :
        tomerge = []
        #OUTCH!!First cluster removed !!
        #print 'ELMINATION CLUSTER 1'
        for i in range(0, self.clnb):
            self.pathout = PathOut(self.parametres['ira'])
            simiparam = DoConf(self.ira.ConfigPath['simitxt']).getoptions()
            simiparam['coeff'] = 3
            simiparam['cexfromchi'] = True
            profclasse = dictprofile[repr(i+1)]
            line1 = profclasse.pop(0)
            classen = [line for line in profclasse if line[0] != '*' and line[0] != '*****']
            try :
                self.lenact = profclasse.index(['*****', '*', '*', '*', '*', '*', '', ''])
                profclasse.pop(self.lenact)
            except ValueError:
                try :
                    self.lenact = profclasse.index(['*', '*', '*', '*', '*', '*', '', ''])
                    profclasse.pop(self.lenact)
                except ValueError:
                    self.lenact = len(profclasse)
            try :
                self.lensup = profclasse.index(['*', '*', '*', '*', '*', '*', '', ''])
                self.lensup = self.lensup - self.lenact
                profclasse.pop(self.lensup)
            except ValueError:
                self.lensup = len(profclasse) - self.lenact
            self.lenet = len(profclasse) - (self.lenact + self.lensup)
#            print self.lenact, self.lensup, self.lenet
            for l,  line in enumerate(classen) :
                line[0] = l
            dictdata = dict(list(zip([l for l in range(0,len(classen))], classen)))
            if self.lenact != 0 :
                self.la = [dictdata[l][6] for l in range(0, self.lenact)]
                self.lchi = [dictdata[l][4] for l in range(0, self.lenact)]
                self.lfreq = [dictdata[l][1] for l in range(0, self.lenact)]
            else :
                self.la = []
                self.lchi = []
                self.lfreq = []
            print('cluster : ', i)
            simi = SimiFromCluster(self.ira, self.corpus, self.la, self.lfreq,
                            self.lchi, i, parametres = simiparam, limit=100)
            tomerge.append(simi.parametres['ira'])
            print(tomerge)
        newparam = {'type': 'merge', 'fileout' : '/tmp/test.txt'}
        newparam['graphs'] = tomerge
        AnalyseMerge(self.ira, newparam, dlg=None)
