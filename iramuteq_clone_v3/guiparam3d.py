# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------

# utilisé seulement dans layout.py

# begin wxGlade: extracode
# end wxGlade



class param3d(wx.Panel):
    def __init__(self, *args, **kwds):
        wx.Panel.__init__(self, *args, **kwds)
        self.label_2 = wx.StaticText(self, -1, "Paramètres")
        self.label_3 = wx.StaticText(self, -1, "Variables")
        self.Check1 = wx.CheckBox(self, -1, "actives")
        self.Check2 = wx.CheckBox(self, -1, "supplémentaires")
        self.Check3 = wx.CheckBox(self, -1, "étoilées")
        self.radio_box_2 = wx.RadioBox(self, -1, "représentation :", choices=["coordonnées", "corrélations"], majorDimension=0, style=wx.RA_SPECIFY_ROWS)
        self.Bind(wx.EVT_RADIOBOX, self.EvtRadioBox2, self.radio_box_2)
        self.var="both"
        self.rep="coordonnees"
        self.__set_properties()
        self.__do_layout()
        # end wxGlade

    def __set_properties(self):
        self.radio_box_2.SetSelection(0)
        self.Check1.SetValue(True)
        self.Check2.SetValue(False)
        self.Check3.SetValue(True)
        # end wxGlade

    def __do_layout(self):
        # begin wxGlade: MyFrame1.__do_layout
        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        sizer_3 = wx.BoxSizer(wx.VERTICAL)
        sizer_4 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5 = wx.BoxSizer(wx.VERTICAL)
        sizer_3.Add(self.label_2, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer_3.Add(wx.StaticLine(self,-1), 0, wx.EXPAND,0)
        sizer_5.Add(self.label_3, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.Check1, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.Check2, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.Check3, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.radio_box_2, 0, wx.ALIGN_LEFT, 0)
        #sizer_4.Add(sizer_5, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        #sizer_4.Add(self.radio_box_2, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer_3.Add(sizer_5, 1, wx.EXPAND|wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer_1.Add(sizer_3, 1, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.SetSizer(sizer_1)
        sizer_1.Fit(self)
        self.Layout()
        # end wxGlade
    
    def EvtRadioBox2(self,event):
        nb=event.GetInt()
        if nb==0:
            self.rep='coordonnees'
        elif nb==1:
            self.rep='correlations'


class simi3d(wx.Panel):
    def __init__(self, *args, **kwds):
        wx.Panel.__init__(self, *args, **kwds)
        self.label_2 = wx.StaticText(self, -1, "Paramètres")
        self.label_7 = wx.StaticText(self, -1, "Nombre de points (0 ou 1 = tous les points)")
        self.spin_1 = wx.SpinCtrl(self, -1, "", min=0, max=5000)
        self.label_3 = wx.StaticText(self, -1, "Coefficient")
        self.Choice_1 = wx.Choice(self, -1, (100,50), choices=['euclidean'])
        self.label_4 = wx.StaticText(self, -1, "Faire apparaître des sphères")
        self.Check_1 = wx.CheckBox(self, -1, "")
        self.label_5 = wx.StaticText(self,-1, "Taille des sphères proportionnelle aux effectifs")
        self.Check_2 = wx.CheckBox(self,-1, "")
        self.label_6 = wx.StaticText(self, -1, "Transparence des sphères")
        self.slider_1 = wx.Slider(self, -1, 10, 1, 100, size = (255,-1), style = wx.SL_HORIZONTAL | wx.SL_AUTOTICKS | wx.SL_LABELS 
            )
        self.label_layout = wx.StaticText(self, -1, 'layout')
        self.Choice_2 = wx.Choice(self, -1, (100,50), choices=['random' ,'cercle', 'fruchterman reingold', 'kamada kawai'])
        self.slider_1.SetTickFreq(5)
        self.movie = wx.CheckBox(self, -1, "faire un film")
        self.__set_properties()
        self.__do_layout()
        # end wxGlade

    def __set_properties(self):
        self.Check_1.SetValue(True)
        self.Check_2.SetValue(True)
        self.Choice_2.SetSelection(2)
        #self.Check3.SetValue(True)
        # end wxGlade

    def __do_layout(self):
        # begin wxGlade: MyFrame1.__do_layout
        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        sizer_3 = wx.BoxSizer(wx.VERTICAL)
        sizer_4 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5 = wx.BoxSizer(wx.VERTICAL)
        sizer_3.Add(self.label_2, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer_3.Add(wx.StaticLine(self,-1), 0, wx.EXPAND,0)

        sizer_5.Add(self.label_7, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.spin_1, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(wx.StaticLine(self,-1), 0, wx.EXPAND,0)
        sizer_5.Add(self.label_3, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.Choice_1, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(wx.StaticLine(self,-1), 0, wx.EXPAND,0)
        sizer_5.Add(self.label_4, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.Check_1, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(wx.StaticLine(self,-1), 0, wx.EXPAND,0)
        sizer_5.Add(self.label_5, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.Check_2, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(wx.StaticLine(self,-1), 0, wx.EXPAND,0)
        sizer_5.Add(self.label_6, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.slider_1, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(wx.StaticLine(self,-1), 0, wx.EXPAND,0)
        sizer_5.Add(self.label_layout, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.Choice_2, 0, wx.ALIGN_LEFT, 0)
        sizer_5.Add(self.movie, 0, wx.ALIGN_LEFT, 0)

        sizer_3.Add(sizer_5, 1, wx.EXPAND, 0)
        sizer_1.Add(sizer_3, 1, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.SetSizer(sizer_1)
        sizer_1.Fit(self)
        self.Layout()
        # end wxGlade
