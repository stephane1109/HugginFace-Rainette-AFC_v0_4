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
from time import sleep

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr, ConstructAfcmPath, ConstructPathOut
from functions import exec_rcode, check_Rresult
from ProfList import *
from dialog import PrefQuestAlc


class AFCMQ():

    def __init__(self, parent, LISTNUMACTIVE, LISTVARSUP):
        #FIXME
        self.parent = parent
        txt = ''
        self.tempgraph = tempfile.mktemp(dir=parent.TEMPDIR)
        #if parent.g_id: rownames = '1'
        #else : rownames = 'NULL'
        #if parent.g_header : header = 'TRUE'
        #else : header = 'FALSE'
        txt += """
        datadm <- read.table("%s", header = TRUE, sep = ';', quote='"', encoding="%s",row.names=1)
        """ % (ffr(parent.tableau.parametre['csvfile']), parent.tableau.parametre['encodage'])
        if len(LISTVARSUP) == 1 :
            strlistsup = str(tuple(LISTVARSUP)).replace(',', '')
        else: 
            strlistsup = str(tuple(LISTVARSUP))
        if len(LISTNUMACTIVE) == 1:
            strlistact = str(tuple(LISTNUMACTIVE)).replace(',', '')
        else:
            strlistact = str(tuple(LISTNUMACTIVE))
        txt += """
        source("%s")
        """ % self.parent.RscriptsPath['Rgraph']
        txt += """
        lact<-c%s+1
        """ % strlistact
        txt += """
        lsup=c%s+1
        """ % strlistsup
        txt += """
        filename<-"%s"
        """ % ffr(self.tempgraph)
        #FIXME : faire une fonction pour le graph
        txt += """
        library(MASS)
        dataact<-datadm[,lact]
        act <- mca(dataact, abbrev = TRUE)
        datasup<-datadm[,lsup]
        sup <- predict(act, datasup, type="factor")
        ftab<-cbind(dataact,datasup)
        #ftab<-
        #library(ca)
        #debs<-ncol(dataact)+1
        #fins<-ncol(dataact)+ncol(datasup)
        #ftab.mjca<-mjca(ftab,supcol=c(debs:fins),nd=3)
        open_file_graph(filename, width = 800, height = 800)
        plot(act)
        dev.off()
        #plot(ftab.mjca)
        #print(ftab.mjca)
        """
        tmpfile = tempfile.mktemp(dir=parent.TEMPDIR)
        tmpscript = open(tmpfile, 'w')
        tmpscript.write(txt)
        tmpscript.close()
        pid = exec_rcode(self.parent.RPath, tmpfile, wait = False)
        while pid.poll() == None :
            sleep(0.2)
        check_Rresult(self.parent, pid)

    def DoLayout(self):
        #FIXME
        txt = '<img src="%s" />' % self.tempgraph
        return txt

#    def OnRGL(self, event):
#        self.parent.text_ctrl_1.write('runrgl\n')
#        RAFC3DRGL = os.path.join(self.PathFile, self.RAFC3DRGL)
#        RunRgl(RAFC3DRGL)


class DoAFCM():

    def __init__(self, parent):
        dlg = PrefQuestAlc(parent, sim = True)
        #dlg = CHDDialog(parent, -1, u"AFCM", size=(350, 400), style=wx.DEFAULT_DIALOG_STYLE)
        dlg.CenterOnParent()
        self.val = dlg.ShowModal()
        if self.val == wx.ID_OK:
            LISTNUMACTIVE = dlg.nactives
            LISTVARSUP = dlg.varsup
            print(LISTNUMACTIVE)
            print(LISTVARSUP)
            afcm = AFCMQ(parent, LISTNUMACTIVE, LISTVARSUP)
            txtgraph = afcm.DoLayout()
            parent.newtab = wx.html.HtmlWindow(parent.nb, -1)
            if "gtk2" in wx.PlatformInfo:
                parent.newtab.SetStandardFonts()
            parent.newtab.SetPage(txtgraph)
            parent.nb.AddPage(parent.newtab, "AFCM")
            parent.nb.SetSelection(parent.nb.GetPageCount() - 1)
            parent.ShowTab(wx.EVT_BUTTON)
