# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
from time import time

#------------------------------------
# import des fichiers du projet
#------------------------------------
from analysetxt import AnalyseText
from OptionAlceste import OptionAlc
from PrintRScript import RchdTxt, ReinertTxtProf, TgenProfScript, ReDoProfScript
from layout import PrintRapport
from chemins import ChdTxtPathOut, PathOut
from functions import DoConf, print_liste, TGen


class Reinert(AnalyseText) :

    def doanalyse(self) :
        self.parametres['type'] = 'alceste'
        self.pathout.basefiles(ChdTxtPathOut)
        self.actives, lim = self.corpus.make_actives_nb(self.parametres['max_actives'], 1)
        self.parametres['eff_min_forme'] = lim
        self.parametres['nbactives'] = len(self.actives)
        uci = False
        if self.parametres['classif_mode'] == 0 :
            lenuc1, lenuc2 = self.corpus.make_and_write_sparse_matrix_from_uc(self.actives, self.parametres['tailleuc1'], self.parametres['tailleuc2'], self.pathout['TableUc1'], self.pathout['TableUc2'], self.pathout['listeuce1'], self.pathout['listeuce2'])
            self.parametres['lenuc1'] = lenuc1
            self.parametres['lenuc2'] = lenuc2
        elif self.parametres['classif_mode'] == 1 :
            self.corpus.make_and_write_sparse_matrix_from_uces(self.actives, self.pathout['TableUc1'], self.pathout['listeuce1'])
        elif self.parametres['classif_mode'] == 2 :
            self.corpus.make_and_write_sparse_matrix_from_uci(self.actives, self.pathout['TableUc1'], self.pathout['listeuce1'])
            uci = True
        Rscript = self.printRscript()
        result = self.doR(Rscript, dlg=self.dlg, message='CHD...')
        if not result :
            return 'NOK'
        self.corpus.make_ucecl_from_R(self.pathout['uce'])
        self.corpus.make_and_write_profile(self.actives, self.corpus.lc, self.pathout['Contout'], uci = uci)
        self.sup, lim = self.corpus.make_actives_nb(self.parametres['max_actives'], 2)
        self.corpus.make_and_write_profile(self.sup, self.corpus.lc, self.pathout['ContSupOut'], uci = uci)
        self.corpus.make_and_write_profile_et(self.corpus.lc, self.pathout['ContEtOut'], uci = uci)
        self.clnb = len(self.corpus.lc)
        self.parametres['clnb'] = self.clnb
        Rscript = self.printRscript2()
        self.doR(Rscript, dlg=self.dlg, message='profils et A.F.C. ...')
        self.time = time() - self.t1
        minutes, seconds = divmod(self.time, 60)
        hours, minutes = divmod(minutes, 60)
        self.parametres['time'] = '%.0fh %.0fm %.0fs' % (hours, minutes, seconds)
        self.print_graph_files()

    def preferences(self) :
        parametres = DoConf(self.parent.ConfigPath['reinert']).getoptions('ALCESTE')
        parametres['corpus'] = self.corpus
        parametres['pathout'] = self.pathout
        parametres['lem'] = self.parametres['lem']
        self.dial = OptionAlc(self.parent, parametres)
        self.dial.CenterOnParent()
        self.dialok = self.dial.ShowModal()
        if self.dialok == 5100 :
            parametres['classif_mode'] = self.dial.radio_box_2.GetSelection()
            parametres['tailleuc1'] = self.dial.spin_ctrl_1.GetValue()
            parametres['tailleuc2'] = self.dial.spin_ctrl_2.GetValue()
            parametres['mincl'] = self.dial.spin_ctrl_4.GetValue()
            parametres['minforme'] = self.dial.spin_ctrl_5.GetValue()
            parametres['nbcl_p1'] = self.dial.spin_nbcl.GetValue()
            parametres['max_actives'] = self.dial.spin_max_actives.GetValue()
            parametres['corpus'] = ''
            parametres['svdmethod'] = self.dial.svdmethod[self.dial.choicesvd.GetSelection()]
            parametres['pathout'] = self.pathout.dirout
            parametres['mode.patate'] = self.dial.check_patate.GetValue()
            DoConf(self.parent.ConfigPath['reinert']).makeoptions(['ALCESTE'], [parametres])
            self.dial.Destroy()
            self.parametres.update(parametres)
            return self.parametres
        else :
            self.dial.Destroy()
            return None

    def printRscript(self) :
        RchdTxt(self.pathout, self.parent.RscriptsPath, self.parametres['mincl'], self.parametres['classif_mode'], nbt=self.parametres['nbcl_p1'] - 1, svdmethod=self.parametres['svdmethod'], libsvdc=self.parent.pref.getboolean('iramuteq', 'libsvdc'), libsvdc_path=self.parent.pref.get('iramuteq', 'libsvdc_path'), R_max_mem=False, mode_patate=self.parametres['mode.patate'])
        return self.pathout['Rchdtxt']

    def printRscript2(self) :
        ReinertTxtProf(self.pathout, self.parent.RscriptsPath, self.clnb, 0.9)
        return self.pathout['RTxtProfGraph']

    def print_graph_files(self) :
        mess_afc = "La position des points n'est peut être pas exacte"
        afc_graph_list = [[os.path.basename(self.pathout['AFC2DL_OUT']), 'Variables actives - coordonnées - 30 points par classes - facteurs 1 / 2 - %s' % mess_afc],
                      [os.path.basename(self.pathout['AFC2DSL_OUT']), 'variables supplémentaires - coordonnées - 30 points par classes - facteurs 1 / 2 - %s' % mess_afc],
                      [os.path.basename(self.pathout['AFC2DEL_OUT']), 'Variables illustratives - Coordonnées - 30 points par classes - facteur 1 / 2 - %s' % mess_afc],
                      [os.path.basename(self.pathout['AFC2DCL_OUT']), 'Classes - Coordonnées - facteur 1 / 2']]
        chd_graph_list = [[os.path.basename(self.pathout['dendro1']), 'dendrogramme à partir de chd1']]
        if self.parametres['classif_mode'] == 0 :
            chd_graph_list.append([os.path.basename(self.pathout['dendro2']), 'dendrogramme à partir de chd2'])
        chd_graph_list.append([os.path.basename(self.pathout['arbre1']), 'chd1'])
        if self.parametres['classif_mode'] == 0 :
            chd_graph_list.append([os.path.basename(self.pathout['arbre2']), 'chd2'])
        print_liste(self.pathout['liste_graph_afc'], afc_graph_list)
        print_liste(self.pathout['liste_graph_chd'], chd_graph_list)
        PrintRapport(self, self.corpus, self.parametres)

class TgenProf(AnalyseText):
    def __init__(self, ira, corpus, parametres, cluster_size):
        self.ira = ira
        self.corpus = corpus
        self.parametres = parametres
        self.pathout = PathOut(dirout = self.parametres['pathout'])
        self.cluster_size = [len(classe) for classe in corpus.lc]
        self.doanalyse()

    def doanalyse(self):
        self.tgen = TGen(path = self.parametres['tgenpath'], encoding = 'utf8')
        self.tgen.read(self.tgen.path)
        #self.parametres['etoiles'].sort()
        self.parametres['tgeneff'] = os.path.join(self.parametres['pathout'], 'tgeneff.csv')
        tgenst = self.corpus.make_tgen_profile(self.tgen.tgen, self.corpus.lc)
        clnames = ['cluster_%03d' % i for i in range(1, len(self.cluster_size) + 1)]
        et = dict(list(zip(clnames, self.cluster_size)))
        tgenst = dict([[line[0], dict(list(zip(clnames, line[1:])))] for line in tgenst])
        self.tgen.writetable(self.parametres['tgeneff'], tgenst, et)
        self.parametres['tgenspec'] = os.path.join(self.parametres['pathout'], 'tgenchi2.csv')
        self.parametres['tgenlemeff'] = os.path.join(self.parametres['pathout'], 'tgenlemeff.csv')
        self.parametres['tgenlemspec'] = os.path.join(self.parametres['pathout'], 'tgenlemchi2.csv')
        tgenlemeff = dict([[lem, dict(list(zip(clnames, self.corpus.tgenlem[lem])))] for lem in self.corpus.tgenlem])
        self.tgen.writetable(self.parametres['tgenlemeff'], tgenlemeff, et)
        self.Rscript = TgenProfScript(self)
        self.Rscript.make_script()
        self.Rscript.write()
        self.doR(self.Rscript.scriptout, dlg = False, message = 'R...')


class ReDoProfile(AnalyseText):

    def __init__(self, ira, corpus, analyses, parametres):
        self.ira = ira
        self.corpus = corpus
        self.parametres = parametres
        self.analyse = analyse
        self.pathout = PathOut(dirout = self.parametres['pathout'])
        #self.cluster_size = [len(classe) for classe in corpus.lc]
        self.doanalyse()

    def Rscript(self) :
       script = ReDoProfScript(self)
