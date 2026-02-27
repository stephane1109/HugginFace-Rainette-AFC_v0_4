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
import tempfile
import time

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ConstructPathOut, ChdTxtPathOut, ConstructAfcUciPath, ffr, PathOut
from functions import sortedby, CreateIraFile, print_liste, exec_rcode, check_Rresult
from PrintRScript import RchdQuest
from layout import OpenCHDS, PrintRapport
from dialog import PrefQuestAlc
from analysematrix import AnalyseMatrix


class AnalyseQuest(AnalyseMatrix):

    def doparametres(self, dlg = None):
        if dlg is not None :
            dial = PrefQuestAlc(self.parent, self.tableau)
            dial.CenterOnParent()
            self.val = dial.ShowModal()
            #parametres = self.tableau.parametre
            if self.val == wx.ID_OK :
                self.parametres['nbcl_p1'] = dial.spin_nbcl.GetValue()
                self.parametres['mincl'] = dial.spin_mincl.GetValue()
                if dial.m_radioBox1.GetSelection() == 1 :
                    self.parametres['listact'] = dial.nactives
                    self.parametres['listsup'] = dial.varsup
                else :
                    self.parametres['formatted'] = 1
            else :
                self.parametres = None
            dial.Destroy()
            # DoQuestAlceste(parent, parametres)

#class DoQuestAlceste(AnalyseMatrix):
    def doanalyse(self):
        #parametres['pathout'] = ConstructPathOut(parent.tableau.parametre['filename'], 'ReinertMatrix')
        #self.parametres = parametres
        #self.parametres['type'] = 'reinertmatrix'
        self.DictForme = {}
        self.DictFormeSup = {}
        self.Min = 10
        self.Linecontent = []
        #self.parent = parent
        #self.RPath = self.parent.PathPath.get('PATHS', 'rpath')
        #self.dictpathout = PathOut(dirout = self.pathout)
        #self.dictpathout = self.pathout
        #self.dictpathout.basefiles(ChdTxtPathOut)
        #self.pathout = self.dictpathout
        self.clnb = ''
        self.ListAct = self.parametres.get('listact', False)
        self.ucecla = ''
        #dlg = wx.ProgressDialog("Traitements",
        #                       "Veuillez patienter...",
        #                       maximum=5,
        #                       parent=self.parent,
        #                       style=wx.PD_APP_MODAL | wx.PD_AUTO_HIDE | wx.PD_ELAPSED_TIME
        #                        )
        #AnalyseMatrix.__init__(self, parent, parent.tableau, self.parametres, dlg = dlg)
        #-----------------------------------------------------------
        #    def doanalyse(self) :
        #------------------------------------------------------------
        #self.dictpathout = self.pathout
        self.pathout.basefiles(ChdTxtPathOut)
        self.tableau.pathout.basefiles(ChdTxtPathOut)
        #self.parent.tableau.dictpathout = self.dictpathout
        self.dlg.Center()
        count = 1
        keepGoing = self.dlg.Update(count)
        #-------------------------------------------------------------------
        count += 1
        self.dlg.Update(count, "passage en O/1")
        if 'formatted' in self.parametres:
            self.tableau.make_01_alc_format(self.pathout['mat01.csv'])
        else:
            self.tableau.make_01_from_selection(self.parametres['listact'], self.parametres['listsup'])
        file = open(self.pathout['listeuce1'], 'w')
        file.write('num uce;num uc\n')
        for i in range(0, len(self.tableau.linecontent)):
            file.write('%i;%i\n' % (i, i))
        file.close()
        self.nbind = len(self.tableau.linecontent)
        #------------------------------------------------------------
        RchdQuest(self.pathout, self.parent.RscriptsPath, self.parametres['nbcl_p1'], self.parametres['mincl'])
        #------------------------------------------------------------
        count += 1
        self.dlg.Update(count, "Analyse (patientez...)")
        pid = exec_rcode(self.parent.RPath, self.pathout['Rchdquest'], wait = False)
        while pid.poll() == None :
            self.dlg.Pulse("Analyse (patientez...)")
            time.sleep(0.2)
        if not check_Rresult(self.parent, pid) :
            if self.dlg :
                self.dlg.Destroy()
            return 'NOK'
        #------------------------------------------------------------
        count += 1
        self.dlg.Update(count, "Ecriture des résultats")
        self.tableau.buildprofil()
        self.clnb = self.tableau.clnb
        self.parametres['clnb'] = self.clnb
        self.ucecla = self.tableau.ucecla
        self.BuildProfile()
        temps = time.time() - self.t1
        PrintRapport(self, self, {}, istxt = False)
        self.tableau.save_tableau(self.pathout['analyse.db'])
        #CreateIraFile(self.dictpathout, self.clnb, corpname = os.path.basename(self.parent.filename), section = 'questionnaire')
        afc_graph_list = [[os.path.basename(self.pathout['AFC2DL_OUT']), 'Variables actives - coordonnées - facteurs 1 / 2'],
                         [os.path.basename(self.pathout['AFC2DSL_OUT']), 'variables illustratives - coordonnées - facteurs 1 / 2'],
                         [os.path.basename(self.pathout['AFC2DCL_OUT']), 'Classes - Coordonnées - facteur 1 / 2'],]
        chd_graph_list = [[os.path.basename(self.pathout['dendro1']), 'dendrogramme à partir de chd1']]
        chd_graph_list.append([os.path.basename(self.pathout['arbre1']), 'chd1'])
        print_liste(self.pathout['liste_graph_afc'], afc_graph_list)
        print_liste(self.pathout['liste_graph_chd'], chd_graph_list)
        #self.tableau = self.parent.tableau
        #OpenCHDS(self.parent, self, self.dictpathout['ira'], False)
        #------------------------------------------------------------
        print('fini', time.time() - self.t1)
        count += 1
        self.dlg.Update(count, "Fini")

    def BuildProfile(self):
        print('build profile')
        txt = ''
        txt += """
        source("%s")
        """ % ffr(self.parent.RscriptsPath['chdfunct'])
        txt += """
        load("%s")
        """ % ffr(self.pathout['RData'])
        txt += """
        dataact<-read.csv2("%s", header = FALSE, sep = ';',quote = '\"', row.names = 1, na.strings = 'NA')
        """ % ffr(self.pathout['Contout'])
        txt += """
        dataet<-read.csv2("%s", header = FALSE, sep = ';',quote = '\"', row.names = 1, na.strings = 'NA')
        """ % ffr(self.pathout['ContEtOut'])
        txt += """
        clnb<-%i
        """ % self.clnb
        txt += """
        tablesqrpact<-BuildProf(as.matrix(dataact),n1,clnb)
        tablesqrpet<-BuildProf(as.matrix(dataet),n1,clnb)
        PrintProfile(n1,tablesqrpact[4],tablesqrpet[4],tablesqrpact[5],tablesqrpet[5],%i,"%s","%s")
        """ % (self.clnb, ffr(self.pathout['PROFILE_OUT']), ffr(self.pathout['ANTIPRO_OUT']))
        txt += """
        colnames(tablesqrpact[[2]])<-paste('classe',1:clnb,sep=' ')
        colnames(tablesqrpact[[1]])<-paste('classe',1:clnb,sep=' ')
        colnames(tablesqrpet[[2]])<-paste('classe',1:clnb,sep=' ')
        colnames(tablesqrpet[[1]])<-paste('classe',1:clnb,sep=' ')
        chistabletot<-rbind(as.data.frame(tablesqrpact[2]),as.data.frame(tablesqrpet[2]))
        ptabletot<-rbind(as.data.frame(tablesqrpact[1]),as.data.frame(tablesqrpet[1]))
        gbcluster<-n1
        write.csv2(chistabletot,file="%s")
        """ % ffr(self.pathout['chisqtable'])
        txt += """
        write.csv2(ptabletot,file="%s")
        """ % ffr(self.pathout['ptable'])
        txt += """
        write.csv2(gbcluster,file="%s")
        """ % ffr(self.pathout['SbyClasseOut'])
        if self.clnb > 2 :
            txt += """
            library(ca)
            rowtot<-nrow(dataact)+nrow(dataet)
            afctable<-rbind(as.matrix(dataact),as.matrix(dataet))
            colnames(afctable)<-paste('classe',1:clnb,sep=' ')
            afc<-ca(afctable,suprow=((nrow(dataact)+1):rowtot),nd=(ncol(afctable)-1))
            debet<-nrow(dataact)+1
            debsup<-NULL
            fin<-rowtot
            afc<-AddCorrelationOk(afc)
            source("%s")
            """ % ffr(self.parent.RscriptsPath['Rgraph'])
            txt += """
            afc <- summary.ca.dm(afc)
            afc_table <- create_afc_table(afc)
            write.csv2(afc_table$facteur, file = "%s")
            write.csv2(afc_table$colonne, file = "%s")
            write.csv2(afc_table$ligne, file = "%s")
            """ % (ffr(self.pathout['afc_facteur']), ffr(self.pathout['afc_col']), ffr(self.pathout['afc_row']))
            txt += """
            xlab <- paste('facteur 1 - ', round(afc$facteur[1,2],2), sep = '')
            ylab <- paste('facteur 2 - ', round(afc$facteur[2,2],2), sep = '')
            xlab <- paste(xlab, ' %', sep = '')
            ylab <- paste(ylab, ' %', sep = '')
            """
            txt += """
            PARCEX<-%s
            """ % "0.9"
            txt += """
            xyminmax <- PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", what='coord', deb=1, fin=(debet-1), xlab = xlab, ylab = ylab)
            """ % (ffr(self.pathout['AFC2DL_OUT']))
            txt += """
            PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", what='coord', deb=debet, fin=fin, xlab = xlab, ylab = ylab, xmin = xyminmax$xminmax[1], xmax = xyminmax$xminmax[2], ymin = xyminmax$yminmax[1], ymax = xyminmax$yminmax[2])
            """ % (ffr(self.pathout['AFC2DSL_OUT']))
            txt += """
            PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", col = TRUE, what='coord', xlab = xlab, ylab = ylab, xmin = xyminmax$xminmax[1], xmax = xyminmax$xminmax[2], ymin = xyminmax$yminmax[1], ymax = xyminmax$yminmax[2])
            """ % (ffr(self.pathout['AFC2DCL_OUT']))
        txt += """
        save.image(file="%s")
        """ % ffr(self.pathout['RData'])
        tmpfile = tempfile.mktemp(dir=self.parent.TEMPDIR)
        tmpscript = open(tmpfile, 'w', encoding='utf8')
        tmpscript.write(txt)
        tmpscript.close()
        pid = exec_rcode(self.parent.RPath, tmpfile, wait = False)
        while pid.poll() == None :
            time.sleep(0.2)
        check_Rresult(self.parent, pid)
        temps = time.time() - self.t1
        self.minutes, self.seconds = divmod(temps, 60)
        self.hours, self.minutes = divmod(self.minutes, 60)
