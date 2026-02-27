# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import tempfile
from time import sleep
import os

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr, FFF
from functions import exec_rcode, check_Rresult


class InputText :

    def __init__(self, parent):
        #wx.Frame.__init__(self, parent, size=wx.Size(400,300))
        self.tempdir = parent.TEMPDIR
        self.fileforR = parent.tableau.parametre['csvfile']
        self.parent=parent
        self.Rpath = parent.PathPath.get('PATHS', 'rpath')
        self.panel = wx.Panel(self.parent.nb, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
        self.Intro = wx.StaticText(self.panel, -1, """dm <- read.csv2("%s", header=TRUE, row.names=1, na.string='')""" % (self.fileforR))
        self.splitter = wx.SplitterWindow(self.panel, -1)
        self.TextPage=wx.TextCtrl(self.splitter, style=wx.TE_MULTILINE | wx.TE_RICH2)
        self.TextPage.SetFont(wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, 0, "courier"))
        self.OutPage = wx.TextCtrl(self.splitter, style=wx.TE_MULTILINE | wx.TE_RICH2 | wx.TE_READONLY)
        self.OutPage.SetFont(wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, 0, "courier"))
        self.OutPage.SetBackgroundColour(wx.BLACK)
        self.OutPage.SetForegroundColour(wx.WHITE)
        self.SimpleCommand =  wx.TextCtrl(self.panel, style=wx.TE_RICH2|wx.TE_PROCESS_ENTER)
        self.SimpleCommand.SetFont(wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, 0, "courier"))
        self.splitter.SplitVertically(self.TextPage, self.OutPage)
        self.button_1 = wx.Button(self.panel, -1, "Executer")
        self.button_2 = wx.Button(self.panel, -1, "Enregistrer...")
        self.__do_layout()
        self.parent.nb.AddPage(self.panel, 'R Code...')
        self.parent.nb.SetSelection(parent.nb.GetPageCount() - 1)
        self.parent.ShowAPane("Tab_content")
        self.panel.Bind(wx.EVT_BUTTON, self.OnSavePage, self.button_2)
        self.panel.Bind(wx.EVT_BUTTON, self.OnExecute, self.button_1)
        self.panel.Bind(wx.EVT_TEXT_ENTER, self.OnEnter, self.SimpleCommand)
        #self.parent.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

    def __do_layout(self):
        sizer_3 = wx.BoxSizer(wx.VERTICAL)
        sizer_1 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        #sizer_1.Add(self.TextPage, 1, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
        #sizer_1.Add(self.OutPage, 1, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
        sizer_3.Add(self.Intro, 0, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.splitter, 1, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
        sizer_2.Add(self.button_1, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
        sizer_2.Add(self.button_2, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
        sizer_3.Add(sizer_1, 5, wx.EXPAND, 0)
        sizer_3.Add(sizer_2, 0, wx.EXPAND, 0)
        sizer_3.Add(self.SimpleCommand, 0, wx.EXPAND, 0)
        #self.SetAutoLayout(True)
        self.panel.SetSizer(sizer_3)
        #self.panel.Layout()

    def OnSavePage(self, evt) :
        dlg = wx.FileDialog(
            self.parent, message="Enregistrer sous...", defaultDir=os.getcwd(),
            defaultFile="script.R", wildcard="Rscript|*.R", style=wx.SAVE | wx.OVERWRITE_PROMPT
            )
        dlg.SetFilterIndex(2)
        dlg.CenterOnParent()
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            with open(path, 'w') as f :
                f.write(self.text)

    def OnExecute(self, event, cmdtxt = False):
        tmpfile = tempfile.mktemp(dir=self.tempdir)
        tempres = tempfile.mktemp(dir=self.tempdir)
        text = """
        sink("%s")
        dm <- read.csv2("%s", header=TRUE, row.names=1, na.string='')
        """ % (ffr(tempres), ffr(self.fileforR))
        end = """
        sink()
        """
        if cmdtxt :
            text = text + cmdtxt + end
        else :
            text = text + self.TextPage.GetValue() + end
        self.text = self.TextPage.GetValue()
        with open(tmpfile, 'w') as tmpscript :
            tmpscript.write(text + text)
        pid = exec_rcode(self.Rpath, tmpfile, wait = False)
        while pid.poll() == None :
            sleep(0.2)
        try :
            check_Rresult(self.parent,pid)
            self.done = True
        except Exception as prob :
            self.done = False
            print('zerzerzerzer', prob)
            mess = wx.MessageDialog(self.parent, prob[1], 'Erreur dans le code R',wx.OK|wx.ICON_INFORMATION)
            mess.ShowModal()
        with open(tempres, 'r') as tmpin :
            res = tmpin.read()
        self.OutPage.write(res)
        self.OutPage.ScrollLines(-1)

    def OnEnter(self, evt) :
        self.OnExecute(evt, self.SimpleCommand.GetValue())
        if self.done :
            self.TextPage.write('\n' + self.SimpleCommand.GetValue())
            self.SimpleCommand.Clear()

    def OnCloseWindow(self, event):
        self.Destroy()
