# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
from copy import copy
from operator import itemgetter
import codecs
import logging

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr, simipath
from analysetxt import AnalyseText
from guifunct import PrepSimi
from functions import indices_simi, progressbar, treat_var_mod, read_list_file, print_liste, DoConf, exec_rcode, check_Rresult
from PrintRScript import PrintSimiScript


log = logging.getLogger('iramuteq.textsimi')


class SimiTxt(AnalyseText):

    def doanalyse(self) :
        self.parametres['type'] = 'simitxt'
        self.pathout.basefiles(simipath)
        self.indices = indices_simi
        if self.dlg :
            self.makesimiparam()
        #FIXME
        self.actives = self.corpus.make_actives_limit(3)
        dictcol = dict([[i, [act, self.corpus.getlemeff(act)]] for i, act in enumerate(self.actives)])
        continu = False
        if self.dlg :
            self.listet = self.corpus.make_etoiles()
            self.listet.sort()
            self.stars = copy(self.listet)
            self.parametres['stars'] = copy(self.listet)
            self.parametres['sfromchi'] = False
            prep = PrepSimi(self.ira, self, self.parametres, self.pathout['selected.csv'], self.actives, indices_simi, wordlist=dictcol)
            if prep.val == wx.ID_OK :
                continu = True
                self.parametres = prep.parametres
                self.dlg = progressbar(self.ira, 4)
            else :
                return False
        else :
            order_actives = [[i, act, self.corpus.getlemeff(act)] for i, act in enumerate(self.actives)]
            order_actives = sorted(order_actives, key=itemgetter(2), reverse = True)
            with open(self.pathout['selected.csv'], 'w', encoding='utf8') as f :
                f.write('\n'.join([repr(order_actives[val][0]) for val in self.parametres['selected']]))
            continu = True
        if continu :
            self.makefiles()
            script = PrintSimiScript(self)
            script.make_script()
            if not self.doR(script.scriptout, dlg = self.dlg, message = 'R...') :
                log.info('Problem')
                return False
            if self.parametres['type_graph'] == 1:
                if self.parametres['svg'] :
                    filename, ext = os.path.splitext(script.filename)
                    fileout = filename + '.svg'
                else :
                    fileout = script.filename
                if os.path.exists(self.pathout['liste_graph']):
                    graph_simi = read_list_file(self.pathout['liste_graph'])
                    graph_simi.append([os.path.basename(fileout), script.txtgraph])
                else :
                    graph_simi = [[os.path.basename(fileout), script.txtgraph]]
                print_liste(self.pathout['liste_graph'], graph_simi)
        else :
            return False

    def makesimiparam(self) :
        self.paramsimi = {'coeff' : 0,
                          'layout' : 2,
                          'type_graph' : 1,
                          'arbremax' : 1,
                          'coeff_tv' : 1,
                          'coeff_tv_nb' : 0,
                          'tvprop' : 0,
                          'tvmin' : 5,
                          'tvmax' : 30,
                          'coeff_te' : 1,
                          'coeff_temin' : 1,
                          'coeff_temax' : 10,
                          'label_v': 1,
                          'label_e': 0,
                          'vcex' : 1,
                          'cexfromchi' : False,
                          'vcexmin' : 10,
                          'vcexmax' : 25,
                          'cex' : 10,
                          'seuil_ok' : 0,
                          'seuil' : 1,
                          'cols' : (255,0,0),
                          'cola' : (200,200,200),
                          'width' : 1000,
                          'height' : 1000,
                          'bystar' : False,
                          'first' : True,
                          'keep_coord' : False,
                          'alpha' : 20,
                          'film': False,
                          'svg' : 0,
                          'com' : 0,
                          'communities' : 0,
                          'halo' : 0,
                          #'ira' : self.pathout['Analyse.ira']
                          }
        self.parametres.update(self.paramsimi)

    def makefiles(self, lim=3) :
        #self.actives, lim = self.corpus.make_actives_nb(self.parametres.get('max_actives',1500), 1)
        self.parametres['eff_min_forme'] = lim
        self.parametres['nbactives'] = len(self.actives)
        self.parametres['fromprof'] = False
        self.corpus.make_and_write_sparse_matrix_from_uces(self.actives, self.pathout['mat01.csv'], self.pathout['listeuce1.csv'])
        with open(self.pathout['actives.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join(self.actives))


class SimiFromCluster(SimiTxt) :

    def __init__(self, ira, corpus, actives, lfreq, lchi, numcluster, parametres = None, dlg = False,  Fromstat=False, limit=None) :
        self.actives = actives
        self.numcluster = numcluster
        self.Fromstat = Fromstat
        self.lfreq = lfreq
        self.lchi = lchi
        self.limit = limit
        parametres['name'] = 'simi_classe_%i' % (numcluster + 1)
        #dlg.Destroy()
        SimiTxt.__init__(self, ira, corpus, parametres, dlg=dlg, lemdial = False)

    def preferences(self) :
        return self.parametres

    def doanalyse(self) :
        self.parametres['type'] = 'clustersimitxt'
        self.pathout.basefiles(simipath)
        self.indices = indices_simi
        self.makesimiparam()
        if 'bystar' in self.parametres :
            del self.parametres['bystar']
        if not self.Fromstat :
            dictcol = dict([[i, [act, self.corpus.getlemclustereff(act, self.numcluster)]] for i, act in enumerate(self.actives)])
        else :
            dictcol = dict([[i, [act, self.corpus.lems[act].freq]] for i, act in enumerate(self.actives)])
        continu = True
        if self.dlg :
#            self.dlg.Destroy()
            dlg = True
            self.dlg.Show(False)
            self.stars = []
            self.parametres['stars'] = 0
            self.parametres['sfromchi'] = 1
            prep = PrepSimi(self.ira, self, self.parametres, self.pathout['selected.csv'], self.actives, indices_simi, wordlist=dictcol)
            if prep.val == wx.ID_OK :
                continu = True
                self.parametres = prep.parametres
                print(self.parametres['com'])
            else :
                continu = False
        else :
            dlg = False
        if continu :
            if dlg :
                self.dlg = progressbar(self.parent, 3)
            else :
                if self.limit is None or self.limit > len(self.actives):
                    self.limit = len(self.actives)
                self.parametres['selected'] = [val for val in range(self.limit)]
                order_actives = [[i, act, self.corpus.getlemeff(act)] for i,
                                 act in enumerate(self.actives) if i <= self.limit]
                #order_actives = sorted(order_actives, key=itemgetter(2), reverse = True)
                with open(self.pathout['selected.csv'], 'w') as f :
                    f.write('\n'.join([repr(order_actives[val][0]) for val in self.parametres['selected']]))
            self.makefiles()
            self.parametres['type'] = 'clustersimitxt'
            script = PrintSimiScript(self)
            script.make_script()
            if not self.doR(script.scriptout, dlg = self.dlg, message = 'R ...') :
                return False
            if self.parametres['type_graph'] == 1:
                if self.parametres['svg'] :
                    filename, ext = os.path.splitext(script.filename)
                    fileout = filename + '.svg'
                else :
                    fileout = script.filename
                if os.path.exists(self.pathout['liste_graph']):
                    graph_simi = read_list_file(self.pathout['liste_graph'])
                    graph_simi.append([os.path.basename(fileout), script.txtgraph])
                else :
                    graph_simi = [[os.path.basename(fileout), script.txtgraph]]
                print_liste(self.pathout['liste_graph'], graph_simi)
            try :
                self.dlg.Destroy()
            except :
                print('dlg is bool')
        else :
            return False

    def makefiles(self) :
        self.parametres['eff_min_forme'] = 3
        self.parametres['nbactives'] = len(self.actives)
        self.parametres['fromprof'] = True
        self.corpus.make_and_write_sparse_matrix_from_classe(self.actives, self.corpus.lc[self.numcluster], self.pathout['mat01.csv'])
        with open(self.pathout['actives.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join(self.actives))
        with open(self.pathout['actives_nb.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join([repr(val) for val in self.lfreq]))
        with open(self.pathout['actives_chi.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join([repr(val) for val in self.lchi]))
