# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import codecs

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from search_list import SearchList
from dialog import SearchDial


class SearchFrame(wx.Frame):

    def __init__(self, parent, id, title, corpus, size=(800, 900)):
        # begin wxGlade: MyFrame.__init__
        #kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, parent, id, size = size, style = wx.CLOSE_BOX|wx.DEFAULT_FRAME_STYLE|wx.TAB_TRAVERSAL)
        self.parent = parent
        self.ira = wx.GetApp().GetTopWindow()
        self.SetIcon(self.ira._icon)
        search_id = wx.NewId()
        self.Bind(wx.EVT_MENU, self.onsearch, id = search_id)
        self.accel_tbl = wx.AcceleratorTable([(wx.ACCEL_CTRL, ord('F'), search_id)])
        self.SetAcceleratorTable(self.accel_tbl)
        self.corpus = corpus
        dlg = wx.ProgressDialog("Traitements", "lecture du tableau...", maximum = 4, parent=self, style = wx.PD_APP_MODAL|wx.PD_AUTO_HIDE|wx.PD_ELAPSED_TIME)
        dlg.Center()
        dlg.Update(1)
        with open(corpus.dictpathout['chisqtable'], 'r', encoding='utf8') as f :
            chisqtable = [line.replace('\n','').replace('"','').replace(',','.').split(';') for line in f]
        first = chisqtable[0]
        first.pop(0)
        chisqtable.pop(0)
        dlg.Update(2)
        self.dchisqtable = dict([[i, [i, line[0]] + [float(val) for val in line[1:]]] for i, line in enumerate(chisqtable)])
        self.dindex = dict([[line[0], i] for i,line in enumerate(chisqtable)]) 
        #self.text_ctrl_1 = wx.TextCtrl(self, -1, "", style=wx.TE_MULTILINE)
        #nbactives = len(self.corpus.actives)
        dlg.Update(3)
        with open(corpus.dictpathout['ContEtOut'], 'r', encoding='utf8') as f :
            nbetoiles = len(f.readlines())
        with open(corpus.dictpathout['Contout'], 'r', encoding='utf8') as f :
            nbactives = len(f.readlines())
        dlg.Update(4, "Ouverture...")
        self.liste = SearchList(self, parent, self.dchisqtable, first, nbactives, nbetoiles) 
        dlg.Destroy()
        #self.HtmlPage = wx.html.HtmlWindow(self, -1)
        #if "gtk2" in wx.PlatformInfo:
        #    self.HtmlPage.SetStandardFonts()
        #self.HtmlPage.SetFonts('Courier', 'Courier')
        self.button_1 = wx.Button(self, -1, "Fermer")
        self.Bind(wx.EVT_BUTTON, self.OnCloseMe, self.button_1)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
        self.SetTitle('Navigation')
        self.SetSize(wx.Size(900,700))
        self.__do_layout()
        # end wxGlade

    def __do_layout(self):
        # begin wxGlade: MyFrame.__do_layout
        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        sizer_2 = wx.BoxSizer(wx.VERTICAL)
        sizer_2.Add(self.liste, 1, wx.EXPAND | wx.ADJUST_MINSIZE, 0)
        #sizer_2.Add(self.HtmlPage, 1, wx.EXPAND | wx.ADJUST_MINSIZE, 0)
        sizer_2.Add(self.button_1, 0, wx.ALIGN_CENTER_HORIZONTAL | wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(sizer_2, 1, wx.EXPAND, 0)
        self.SetAutoLayout(True)
        self.SetSizer(sizer_1)
        self.Layout()
        # end wxGlade

    def OnCloseMe(self, event):
        self.Show(False)

    def OnCloseWindow(self, event):
        self.Show(False)

    def onsearch(self, evt) :
        if evt is not None :
            self.dial = SearchDial(self, self.liste, 1, True)
            self.dial.Show()
            #self.dial.Destroy()
        else :
            self.dial = SearchDial(self, self.liste, 1, False)
