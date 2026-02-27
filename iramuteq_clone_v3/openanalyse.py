# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import logging

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ChdTxtPathOut, StatTxtPathOut, PathOut
from layout import OpenCHDS, dolexlayout, StatLayout, WordCloudLayout, OpenCorpus, SimiLayout, SimiMatLayout, ProtoLayout, MatLayout, FreqLayout, Chi2Layout, LabbeLayout, CateLayout
from corpus import Corpus, copycorpus
from tableau import Tableau
from functions import DoConf, ReadDicoAsDico


log = logging.getLogger('iramuteq.openanalyse')


class OpenAnalyse():

    def __init__(self, parent, parametres, Alceste=True, simifromprof = False):
        log.info('OpenAnalyse')
        self.parent = parent
        if isinstance(parametres, dict) :
            self.conf = DoConf(parametres['ira']).getoptions()
            self.path = parametres['ira']
        else :
            self.conf = DoConf(parametres).getoptions()
            self.path = parametres
            self.conf = self.redopath(self.conf, parametres)
        if self.conf['type'] == 'corpus' :
            corpus = self.opencorpus()
        elif self.conf['type'] == 'matrix' :
            matrix = self.openmatrix()
        elif self.conf.get('corpus', False) in self.parent.history.corpus :
            if self.conf['uuid'] in self.parent.history.analyses :
                intree  = True
            else :
                intree = False
            corpus = self.openanalyse()
            if self.conf.get('lem',1) :
                dolem = True
            else :
                dolem = False
            if self.conf.get('dictionary', False) :
                dico = ReadDicoAsDico(self.conf['dictionary'])
                corpus.make_lems_from_dict(dico, dolem = dolem)
            else :
                corpus.make_lems(lem = dolem)
            if not intree :
                self.parent.tree.AddAnalyse(self.conf, bold = True)
            else :
                self.parent.tree.GiveFocus(uuid = self.conf['uuid'], bold = True)
            self.doopen(corpus)
        elif self.conf.get('matrix', False) in self.parent.history.ordermatrix :
            corpus = None
            matrix = Tableau(self.parent, parametres = self.parent.history.matrix[self.parent.history.ordermatrix[self.conf['matrix']]])
            matrix.open()

            #if isinstance(parametres, dict) :
            #    tableau = Tableau(parent, parametres['ira'])
            #else :
            #    tableau = Tableau(parent, parametres)
            #tableau.parametres = self.conf 
            #tableau.dictpathout = PathOut(filename = tableau.parametres['filename'], dirout = self.conf['pathout'], analyse_type = self.conf['type'])
            #tableau.dictpathout.basefiles(ChdTxtPathOut)
            #tableau.read_tableau(tableau.dictpathout['db'])
            #if self.parent.tree.IsInTree(uuid = self.conf['uuid']) :

            self.parent.tree.GiveFocus(uuid = self.conf['uuid'], bold = True)
            self.doopen(matrix)
        else :
            self.parent.tree.AddAnalyse(self.conf, bold = True)
        self.parent.history.addtab(self.conf)

    def redopath(self, conf, path) :
        conf['ira'] = os.path.realpath(path)
        conf['pathout'] = os.path.dirname(os.path.realpath(path))
        DoConf(conf['ira']).makeoptions([conf['type']], [conf])
        return conf

    def opencorpus(self) :
        log.info('open corpus')
        if self.conf['uuid'] not in self.parent.history.corpus :
            self.parent.history.add(self.conf)
            log.info('add corpus to history')
            self.parent.tree.OnItemAppend(self.conf)
        if self.conf['uuid'] in self.parent.history.openedcorpus :
            log.info('corpus is already opened')
            self.doopen(self.parent.history.openedcorpus[self.conf['uuid']])
        else :
            #dial = progressbar(2)
            #dial.Update(1, 'Ouverture du corpus')
            corpus = Corpus(self, parametres = self.conf, read = self.parent.history.history[self.parent.history.ordercorpus[self.conf['uuid']]]['ira'])
            #dial.Update(2, 'Fini')
            #dial.Destroy()
            self.parent.history.openedcorpus[self.conf['uuid']] = corpus
            self.opencorpus_analyses()
            self.doopen(corpus)

    def openmatrix(self):
        log.info('open matrix')
        if self.conf['uuid'] not in self.parent.history.ordermatrix :
            self.parent.history.addMatrix(self.conf)
            log.info('add matrix to history')
            self.parent.tree.OnItemAppend(self.conf)
        if self.conf['uuid'] in self.parent.history.openedmatrix :
            log.info('matrix is already opened')
            self.doopen(self.parent.history.openedmatrix[self.conf['uuid']])
        else :
            #dial = progressbar(2)
            #dial.Update(1, 'Ouverture du corpus')
            matrix = Tableau(self, parametres = self.conf)
            matrix.open()
            self.parent.history.openedmatrix[self.conf['uuid']] = matrix
            self.openmatrix_analyses()
            self.doopen(matrix)
            self.parent.history.addtab(self.conf)

    def opencorpus_analyses(self) :
        log.info('open analysis')
        basepath = self.conf['pathout']
        analyses = []
        for root, subfolders, files in os.walk(basepath) :
            for folder in subfolders :
                if os.path.exists(os.path.join(folder, 'Analyse.ira')) :
                    analyse_conf = DoConf(os.path.join(folder, 'Analyse.ira')).getoptions()
                    analyse_conf = self.redopath(analyse_conf, os.path.join(folder, 'Analyse.ira'))
                    if analyse_conf['corpus'] == self.conf['uuid'] :
                        analyses.append(analyse_conf)
        if len(analyses) :
            self.parent.history.addmultiple(analyses)
        for analyse in analyses :
            self.parent.tree.AddAnalyse(analyse, bold = False)

    def openmatrix_analyses(self):
        pass

    def openanalyse(self) :
        if self.conf['corpus'] in self.parent.history.openedcorpus :
            log.info('corpus is already opened')
            corpus = copycorpus(self.parent.history.openedcorpus[self.conf['corpus']])
        else :
            if os.path.exists(self.parent.history.history[self.parent.history.ordercorpus[self.conf['corpus']]]['ira']) :
                corpus = Corpus(self, parametres = DoConf(self.parent.history.history[self.parent.history.ordercorpus[self.conf['corpus']]]['ira']).getoptions('corpus'), read = self.parent.history.history[self.parent.history.ordercorpus[self.conf['corpus']]]['ira'])
                self.parent.history.openedcorpus[self.conf['corpus']] = corpus
        self.parent.history.add(self.conf)
        return corpus

    def doopen(self, corpus) :
        if self.conf['type'] == 'corpus' :
            OpenCorpus(self.parent, self.conf)
        elif self.conf['type'] == 'stat' :
            StatLayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'spec' :
            dolexlayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'labbe' :
            LabbeLayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'alceste' :
            OpenCHDS(self.parent,  corpus, self.conf, Alceste = True)
        elif self.conf['type'] == 'simitxt' or self.conf['type'] == 'clustersimitxt' :
            SimiLayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'wordcloud' or self.conf['type'] == 'clustercloud':
            WordCloudLayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'reinertmatrix' :
            OpenCHDS(self.parent,  corpus, self.conf, Alceste = False)
        elif self.conf['type'] == 'simimatrix' or self.conf['type'] == 'simiclustermatrix':
            SimiMatLayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'proto' :
            ProtoLayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'matrix' :
            MatLayout(self.parent, corpus)
        elif self.conf['type'] == 'freq' or self.conf['type'] == 'freqmulti':
            FreqLayout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'chi2' or self.conf['type'] == 'chi2mcnemar':
            Chi2Layout(self.parent, corpus, self.conf)
        elif self.conf['type'] == 'categorisation' :
            CateLayout(self.parent, corpus, self.conf)
            #print(self.conf)
