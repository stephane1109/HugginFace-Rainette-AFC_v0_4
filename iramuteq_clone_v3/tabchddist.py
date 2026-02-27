# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import tempfile
import time

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr, ConstructPathOut,ChdTxtPathOut
from functions import CreateIraFile, print_liste, exec_rcode, check_Rresult
from dialog import PrefQuestAlc, ClusterNbDialog


def RchdFunct(self,parent, rep_out, CLASSIF, encode, RscriptsPath):
    SEUIL_CHI2_PROF=2
    txt=""
    txt+="""
    source("%s")
    """%RscriptsPath['chdfunct']
    txt+="""
    dataact<-read.csv2("%s", header = FALSE, sep = ';',quote = '\"', na.strings = '')
    """%ffr(rep_out['FILE_ACT_TEMP'])
    txt+="""
    dataet<-read.csv2("%s", header = FALSE, sep = ';',quote = '\"', na.strings = '')
    """%ffr(rep_out['FILE_ET_TEMP'])
    
    fileout=tempfile.mktemp(dir=parent.TEMPDIR)
    
    txt+="""
    dissmat<-daisy(dataact, metric = 'gower', stand = FALSE)
    chd<-diana(dissmat,diss=TRUE,)
    height<-chd$height
    sortheight<-sort(height,decreasing=TRUE)
    BestCLusterNb<-as.matrix(FindBestCluster(sortheight))
    write.csv2(BestCLusterNb,file="%s",row.names=FALSE)
    """%ffr(fileout)
    txt+="""
    save.image(file="%s")
    """%rep_out['RData']
    Rtmp=tempfile.mktemp(dir=parent.TEMPDIR)
    file=open(Rtmp,'w')
    file.write(txt)
    file.close()
    pid = exec_rcode(parent.RPath, Rtmp, wait = False)
    while pid.poll() == None :
        time.sleep(0.2)
    check_Rresult(parent, pid)
    f=open(fileout,'r')
    lcl=f.readlines()
    f.close()
    ListClasseOk=[line.replace('\n','').replace('"','') for line in lcl]
    ListClasseOk.pop(0)
    clusterdlg = ClusterNbDialog(ListClasseOk, parent, -1, "Nombre de classe", size=(350, 200),
                     style = wx.DEFAULT_DIALOG_STYLE
                     )
    clusterdlg.CenterOnParent()
    # this does not return until the dialog is closed.
    val = clusterdlg.ShowModal()
    if val == wx.ID_OK:
        if type(ListClasseOk)!=float :
            CLASSE_CH=ListClasseOk[clusterdlg.list_box_1.GetSelection()]
        else :
            CLASSE_CH=ListClasseOk
    else:
        print("You pressed Cancel\n")
    clusterdlg.Destroy()
    ClusterNb=int(CLASSE_CH)
    txt=''
    txt+="""
    load("%s")
    """%rep_out['RData']
    txt += """
    source("%s")
    """% RscriptsPath['chdfunct']
    txt+="""
    clnb<-%i
    """%ClusterNb
    txt+="""
    classes<-as.data.frame(cutree(as.hclust(chd), k=clnb))[,1]
    datatot<-cbind(dataact,dataet)
    dataclasse<-cbind(datatot,classes)
    dataactclasses<-cbind(dataact,classes)
    dataetclasses<-cbind(dataet,classes)
    afctableact<-BuildContTable(dataactclasses)
    afctableet<-BuildContTable(dataetclasses)
    tablesqrpact<-BuildProf(afctableact,dataactclasses,clnb)
    tablesqrpet<-BuildProf(afctableet,dataetclasses,clnb)
    chistabletot<-rbind(as.data.frame(tablesqrpact[2]),as.data.frame(tablesqrpet[2]))
    ptabletot<-rbind(as.data.frame(tablesqrpact[1]),as.data.frame(tablesqrpet[1]))
    cont_out<-rbind(as.data.frame(tablesqrpact[3]),as.data.frame(tablesqrpet[3]))
    colnames(chistabletot)<-paste('classe',1:clnb,sep=' ')
    colnames(ptabletot)<-paste('classe',1:clnb,sep=' ')
    cont_out <- cont_out[,-ncol(cont_out)]
    colnames(cont_out)<-paste('classe',1:clnb,sep=' ')
    write.csv2(chistabletot,file="%s")
    """%rep_out['chisqtable']
    txt+="""
    write.csv2(ptabletot,file="%s")
    """%rep_out['ptable']
    txt+="""
    write.csv2(cont_out,file="%s")
    """%rep_out['Contout']
    txt+="""
    PrintProfile(dataclasse,tablesqrpact[4],tablesqrpet[4],tablesqrpact[5],tablesqrpet[5],clnb,"%s","%s")
    """%(rep_out['PROFILE_OUT'],rep_out['ANTIPRO_OUT'])
    txt+="""
    gbcluster<-dataclasse[ncol(dataclasse)]
    write.csv2(gbcluster,file="%s")
    """%rep_out['SbyClasseOut']
    txt+="""
    library(ca)
    library(cluster)
    afctable<-rbind(afctableact,afctableet)
    colnames(afctable)<-paste('classe',1:clnb,sep=' ')
    afc<-ca(afctable,suprow=((nrow(afctableact)+1):nrow(cont_out)),nd=(ncol(afctable)-1))
    debet<-nrow(afctableact)+1
    fin<-nrow(cont_out)
    source("%s")
    debsup<-NULL
    afc<-AddCorrelationOk(afc)
    afc <-  summary.ca.dm(afc)
    afc_table <- create_afc_table(afc)
    """%RscriptsPath['Rgraph']
    txt+="""
    dendo <- as.dendrogram(as.hclust(chd))
    hthr<-sortheight[clnb]
    dendocut<-cut(dendo,h=hthr)
    save.image("%s")
    """%rep_out['RData']
#    txt+="""
#    PARCEX<-%s
#    """%'0.9'
#    txt+="""
#    PlotDendroComp(chd,"%s",200)
#    """%rep_out['DENDROCOMP_OUT']
#    txt+="""
#    PlotDendroHori(dendocut$upper,"%s",200)
#    """%rep_out['DENDROH_OUT']
#    txt+="""
#    PlotDendroCut(chd,"%s",200,clnb)
#    """%rep_out['DENDROCUT_OUT'] 
    txt += """
    PARCEX<-%s
    """ % "0.9"
    txt += """
    PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", what='coord', deb=1, fin=(debet-1))
    """ % (rep_out['AFC2DL_OUT'])
    txt += """
    PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", what='coord', deb=debet, fin=fin)
    """ % (rep_out['AFC2DSL_OUT'])
    txt += """
    PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", col = TRUE, what='coord')
    """ % (rep_out['AFC2DCL_OUT'])
    txt += """
    PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", what='crl', deb=1, fin=(debet-1))
    """ % (rep_out['AFC2DCoul'])
    txt += """
    PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", what='crl', deb=debet, fin=fin)
    """ % (rep_out['AFC2DCoulSup'])
    txt += """
    PlotAfc2dCoul(afc, as.data.frame(chistabletot), "%s", col = TRUE, what='crl')
    """ % (rep_out['AFC2DCoulCl'])
    f=open(Rtmp,'w')
    f.write(txt)
    f.close()
    pid = exec_rcode(parent.RPath, Rtmp, wait = False)
    while pid.poll() == None :
        time.sleep(0.2)
    check_Rresult(parent, pid)
    return ClusterNb


class AnalyseCHDS():

    def __init__(self, parent, numactives, varsup):
        self.t1=time.time()
        #------------------------------------------------------
        self.dlg=wx.ProgressDialog("Traitements",
                               "Veuillez patienter...",
                               maximum = 7,
                               parent=parent,
                               style = wx.PD_APP_MODAL|wx.PD_AUTO_HIDE|wx.PD_ELAPSED_TIME
                                )
        self.dlg.Center()
        self.count = 1
        keepGoing = self.dlg.Update(self.count)
        #-------------------------------------------------------
        self.Filename=parent.filename
        self.parent=parent
        self.encode=parent.encode
        self.numactives=numactives
        self.varsup=varsup
        #-------------------------------------------------------
        self.count += 1
        keepGoing = self.dlg.Update(self.count)
        #self.OnAnalyse()
        #-------------------------------------------------------

    def OnAnalyse(self):
        PathOut=ConstructPathOut(self.parent.tableau.parametre['filename'],'CHDS')
        self.pathout = PathOut
        dictpathout = ChdTxtPathOut(PathOut)
        self.dictpathout=dictpathout
        self.parent.tableau.dictpathout = dictpathout
        self.RPath=self.parent.PathPath.get('PATHS','rpath')
        #-------------------------------------------------------
        self.count += 1
        keepGoing = self.dlg.Update(self.count,"lecture des données")
        colact = self.parent.tableau.select_col(self.numactives)
        colsup = self.parent.tableau.select_col(self.varsup)
        self.parent.tableau.make_01_from_selection(self.numactives, self.varsup, False)
        dictpathout['FILE_ACT_TEMP']=tempfile.mktemp(dir=self.parent.TEMPDIR)
        savetxt(dictpathout['FILE_ACT_TEMP'],colact,fmt='%s',delimiter=';')
        dictpathout['FILE_ET_TEMP']=tempfile.mktemp(dir=self.parent.TEMPDIR)
        savetxt(dictpathout['FILE_ET_TEMP'],colsup,fmt='%s',delimiter=';')
        #-------------------------------------------------------
        self.count += 1
        keepGoing = self.dlg.Update(self.count,"Analyse (patientez...)")
        #------------FIXME----------
        clnb=RchdFunct(self,self.parent,dictpathout, 'DIANA',self.parent.SysEncoding,self.parent.RscriptsPath)
        self.clnb=clnb
        #-------------------------------------------------------
        self.count += 1
        keepGoing = self.dlg.Update(self.count,"Ecriture des résultats")
        return dictpathout,clnb

    def PrintResult(self,dictpathout,clnb):
        with open(self.dictpathout['SbyClasseOut'], 'r') as filein :
            content = filein.readlines()
        content.pop(0)
        for i, line in enumerate(content) :
            line = line.replace('\n', '').replace('"', '').split(';')
            self.parent.tableau.classes.append([int(line[0]) - 1, int(line[1])])        
        temps=time.time()-self.t1
        self.minutes, self.seconds = divmod(temps, 60)
        self.hours, self.minutes = divmod(self.minutes, 60)  
        #PrintRapport(self,'quest_simi')
        self.parent.tableau.dictpathout = self.dictpathout
        self.parent.tableau.save_tableau(self.dictpathout['db'])
        CreateIraFile(dictpathout,clnb, corpname = os.path.basename(self.Filename), section = 'chd_dist_quest')
        #-------------------------------------------------------
        self.count += 1
        keepGoing = self.dlg.Update(self.count,"Ouverture...")
        afc_graph_list = [[os.path.basename(self.dictpathout['AFC2DL_OUT']), 'Variables actives - coordonnées - facteurs 1 / 2'],
                          [os.path.basename(self.dictpathout['AFC2DSL_OUT']), 'variables illustratives - coordonnées - facteurs 1 / 2'],
                          [os.path.basename(self.dictpathout['AFC2DCL_OUT']), 'Classes - Coordonnées - facteur 1 / 2'],
                          [os.path.basename(self.dictpathout['AFC2DCoul']), 'Variables actives - Corrélation - facteur 1/2'],
                          [os.path.basename(self.dictpathout['AFC2DCoulSup']), 'Variables illustratives - Corrélation - facteur 1 / 2'],
                          [os.path.basename(self.dictpathout['AFC2DCoulCl']), 'Classes - Corrélations - facteurs 1 / 2'], ]
        chd_graph_list = [[os.path.basename(self.dictpathout['dendro1']), 'dendrogramme à partir de chd1']]
        #chd_graph_list.append(['arbre1', 'chd1'])
        print_liste(self.dictpathout['liste_graph_afc'], afc_graph_list)
        print_liste(self.dictpathout['liste_graph_chd'], chd_graph_list)
        self.tableau = self.parent.tableau
        OpenAnalyse(self.parent, dictpathout['ira'], False)
        #-------------------------------------------------------
        self.count += 1
        keepGoing = self.dlg.Update(self.count,"Fini")


class ChdCluster():

    def __init__(self,parent):
        dlg = PrefQuestAlc(parent, sim = True)
        #dlg = CHDDialog(parent,-1, u"Classification", size=(350, 400),style = wx.DEFAULT_DIALOG_STYLE)
        dlg.CenterOnParent()
        self.val = dlg.ShowModal()
        if self.val==wx.ID_OK :
            numactives=dlg.nactives
            varsup=dlg.varsup
            chd=AnalyseCHDS(parent, numactives, varsup)
            dictpathout,clnb=chd.OnAnalyse()
            chd.PrintResult(dictpathout,clnb)
            parent.ShowTab(wx.EVT_BUTTON)
