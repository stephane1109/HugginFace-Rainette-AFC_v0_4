# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import datetime
import sys
import tempfile
from time import sleep
import shutil
import codecs
import logging

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.agw.hyperlink as hl
import wx.lib.agw.aui as aui
import wx.lib.agw.labelbook as LB
from wx.lib.agw.fmresources import *

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ConstructPathOut, ChdTxtPathOut, FFF, ffr, PathOut, StatTxtPathOut, simipath
from configparser import ConfigParser
from functions import ReadProfileAsDico, GetTxtProfile, read_list_file, ReadList, exec_rcode, print_liste, BugReport, DoConf,\
 indices_simi, check_Rresult, progressbar, normpath_win32, TGen, ReadList, launchcommand, read_dist_list, readliststat
from ProfList import ProfListctrlPanel
from guiparam3d import param3d, simi3d
from PrintRScript import write_afc_graph, print_simi3d, PrintSimiScript
from profile_segment import ProfileSegment
from listlex import *
from Liste import *
from elcategorizator import ElCategorizator
from search_tools import SearchFrame
from dialog import PrefGraph, PrefExport, PrefSimpleFile, PrefDendro, SimpleDialog, ImageViewer
from guifunct import SelectColumn, PrepSimi, PrefSimi, redosimi
from webexport import WebExport
from corpus import Corpus
from sheet import MySheet
from graph_to_json import GraphToJson
#from listlex2 import LexPanel


import langue
langue.run()



log = logging.getLogger('iramuteq.layout')

def scale_bitmap(bitmap, width, height):
    image = bitmap.ConvertToImage()
    image = image.Scale(width, height, wx.IMAGE_QUALITY_HIGH)
    result = wx.Bitmap(image)
    return result

class GraphPanelAfc(wx.Panel):

    def __init__(self, parent, dico, list_graph, clnb, itempath = 'liste_graph_afc', coding = sys.getdefaultencoding(), islex = False):
        wx.Panel.__init__(self,parent)
        self.afcnb = 1
        self.clnb = clnb
        self.Dict = dico
        self.coding = coding
        self.itempath = itempath
        self.parent = self.GetParent()
        self.SetFont(wx.Font(10, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, 0, "Arial"))
        self.labels = []
        self.listimg = []
        self.buts = []
        self.list_graph = list_graph
        self.islex = islex
        self.TabCHD = self.parent.GetParent()
        self.nb = self.TabCHD.GetParent()
        self.ira = self.nb.GetParent()
        self.panel_1 = wx.ScrolledWindow(self, -1, style=wx.TAB_TRAVERSAL)
        afc_img = wx.Image(os.path.join(self.ira.images_path,'button_afc.jpg'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        self.butafc = wx.BitmapButton(self, -1, afc_img)
        self.Bind(wx.EVT_BUTTON, self.afc_graph, self.butafc)
        self.dirout = os.path.dirname(self.Dict['ira'])
        b = 0
        todel = []
        for i in range(0,len(list_graph)):
            if os.path.exists(os.path.join(self.dirout,list_graph[i][0])) :
                filename, ext = os.path.splitext(list_graph[i][0])
                if ext == '.svg' or ext == '.html':
                    self.listimg.append(hl.HyperLinkCtrl(self.panel_1, -1, os.path.join(self.dirout,list_graph[i][0]), URL=os.path.join(self.dirout,list_graph[i][0])))
                else :
                    path = os.path.join(self.dirout,list_graph[i][0])
                    bitmap = wx.Bitmap(path, wx.BITMAP_TYPE_ANY)
                    #bitmap = scale_bitmap(bitmap, 2000, 2000)
                    #self.listimg.append(wx.StaticBitmap(self.panel_1, -1, wx.Bitmap(os.path.join(self.dirout,list_graph[i][0]), wx.BITMAP_TYPE_ANY)), name=repr(i-b)))
                    self.listimg.append(wx.StaticBitmap(self.panel_1, -1, bitmap, name=repr(i-b)))
                    self.listimg[-1].Bind(wx.EVT_RIGHT_DOWN, self.onrightclick)
                if os.path.exists(os.path.join(self.dirout,list_graph[i][0] + '_notplotted.csv')) :
                    txt = _("List of not plotted points : ") + '%s' % os.path.join(self.dirout,list_graph[i][0] + '_notplotted.csv')
                else :
                    txt = ''
                self.labels.append(wx.StaticText(self.panel_1, -1, list_graph[i][1] + txt))
                self.buts.append(wx.Button(self.panel_1, wx.ID_DELETE, name = repr(i - b)))
            else :
                todel.append(i)
                b += 1
        self.list_graph = [graph for i, graph in enumerate(self.list_graph) if i not in todel]
        self.param = { 'typegraph' : 0,
              'width' : 800,
              'height' : 800,
              'what' : 0,
              'qui' : 0,
              'do_select_nb' : 0,
              'do_select_chi' : 0,
              'do_select_chi_classe' : 0,
              'select_nb' : 50,
              'select_chi' : 4,
              'nbchic' : 30,
              'over' : 0,
              'cex_txt' : 0,
              'txt_min' : 5,
              'txt_max' : 40,
              'tchi' : 0,
              'tchi_min' : 5,
              'tchi_max' : 40,
              'taillecar' : 9,
              'facteur' : [1,2,3],
              'alpha' : 10,
              'clnb' : clnb,
              'svg' : 0,
               }
        if self.islex :
            self.param['islex'] = 1
        else :
            self.param['islex'] = 0
        self.__set_properties()
        self.__do_layout()

    def __set_properties(self):
        self.panel_1.EnableScrolling(True,True)
        #self.panel_1.SetSize((1000,1000))
        self.panel_1.SetScrollRate(20, 20)
        self.panel_1.SetFocus()

    def __do_layout(self):
        self.sizer_1 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer_3 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_2.Add(self.butafc, 0, 0, 0)
        for i in range(0, len(self.listimg)):
            self.sizer_3.Add(self.listimg[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
            self.sizer_3.Add(self.labels[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
            self.sizer_3.Add(self.buts[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
            self.Bind(wx.EVT_BUTTON, self.on_delete_image, self.buts[i])
        self.panel_1.SetSizer(self.sizer_3)
        self.sizer_2.Add(self.panel_1, 1, wx.EXPAND, 0)
        self.SetSizer(self.sizer_2)

    def on_delete_image(self, event) :
        image_id = int(event.GetEventObject().GetName())
        image_path = self.list_graph[image_id][0]
        message = _('This file will be delete : ') + '%s.\n' % os.path.join(self.dirout, image_path) + _('Are you sure ?')
        dial = wx.MessageDialog(self, message, style = wx.YES_NO)
        res = dial.ShowModal()
        if res == wx.ID_YES :
            dial.Destroy()
            log.info('delete image %i' % image_id)
            oldimg = self.listimg.pop(image_id)
            oldimg.Destroy()
            oldlab = self.labels.pop(image_id)
            oldlab.Destroy()
            oldbut = self.buts.pop(image_id)
            oldbut.Show(False)
            for i, but in enumerate(self.buts) :
                but.SetName(repr(i))
                self.listimg[i].SetName(repr(i))
            todel = self.list_graph.pop(image_id)
            os.remove(os.path.join(self.dirout, todel[0]))
            print_liste(self.Dict[self.itempath], self.list_graph)
            self.sizer_3.Fit(self.panel_1)
            self.Layout()
        else :
            dial.Destroy()

    def onrightclick(self, event):
        image_id = int(event.GetEventObject().GetName())
        image_path = self.list_graph[image_id][0]
        viewer = ImageViewer(self, {'tmpgraph' : os.path.join(self.dirout,image_path), 'svg': 'FALSE', 'wildcard': '*.*'}, self.labels[image_id].GetLabelText(), self.listimg[image_id].GetSize())
        #viewer.Show()
        #print image_path
        #print self.labels[image_id].GetLabelText()

    def afc_graph(self,event):
        #dirout = os.path.dirname(self.Dict['ira'])
        dial = PrefGraph(self.parent,-1,self.param,'')
        dial.CenterOnParent()
        val = dial.ShowModal()
        if val == wx.ID_OK :
            if dial.choix_format.GetSelection() == 0 :
                svg = 0
            else :
                svg = 1
            typegraph = dial.choicetype.GetSelection()
            if svg :
                typefile = '.svg'
            else :
                typefile = '.png'
            if self.clnb <= 3 and typegraph == 1 :
                typegraph = 2
            if typegraph == 2:
                typefile = '.gexf'
            if typegraph == 3 :
                typefile = ''
            while os.path.exists(os.path.join(self.dirout,'graph_afc_'+str(self.afcnb)+typefile)):
                self.afcnb +=1
            self.fileout = ffr(os.path.join(self.dirout,'graph_afc_'+str(self.afcnb)+typefile))

            self.param = {'typegraph' : typegraph,
                          'width' : dial.spin1.GetValue(),
                          'height' : dial.spin2.GetValue(),
                          'what' : dial.choice1.GetSelection(),
                          'qui' : dial.choice2.GetSelection(),
                          'do_select_nb' : dial.check1.GetValue(),
                          'do_select_chi' : dial.check2.GetValue(),
                          'do_select_chi_classe' : dial.check_chic.GetValue(),
                          'select_nb' : dial.spin_nb.GetValue(),
                          'select_chi' : dial.spin_chi.GetValue(),
                          'nbchic' : dial.spin_nbchic.GetValue(),
                          'over' : dial.check3.GetValue(),
                          'cex_txt' : dial.check4.GetValue(),
                          'txt_min' : dial.spin_min.GetValue(),
                          'txt_max' : dial.spin_max.GetValue(),
                          'tchi' : dial.check_tchi.GetValue(),
                          'tchi_min' : dial.spin_min_tchi.GetValue(),
                          'tchi_max' : dial.spin_max_tchi.GetValue(),
                          'taillecar' : dial.spin3.GetValue(),
                          'facteur' : [dial.spin_f1.GetValue(),dial.spin_f2.GetValue(), dial.spin_f3.GetValue()],
                          'clnb' : self.clnb,
                          'film' : str(dial.film.GetValue()).upper(),
                          'alpha' : dial.slider_sphere.GetValue(),
                          'svg' : svg
                        }
            if self.islex :
                self.param['islex'] = 1
                if self.param['qui'] == 1 :
                    self.param['qui'] = 3
            else :
                self.param['islex'] = 0
            self.nb.parent = self.ira
            self.DictPathOut = self.Dict
            self.RscriptsPath = self.ira.RscriptsPath
            txt = """
            load("%s")
            """ % ffr(self.DictPathOut['RData'])
            if self.itempath == 'liste_graph_afcf' :
                txt += """
                afc <- afcf
                afc_table <- afcf_table
                chistabletot <- specfp
                """
            elif self.itempath == 'liste_graph_afct' :
                txt +="""
                afc <- afct
                afc_table <- afct_table
                chistabletot <- spectp
                """
            txt += write_afc_graph(self)
            filetmp = tempfile.mktemp()
            with open(filetmp, 'w', encoding='utf8') as f :
                f.write(txt)
            pid = exec_rcode(self.ira.RPath, filetmp)
            check_Rresult(self.ira, pid)
            if self.param['islex'] and self.param['qui'] == 3 :
                self.param['qui']=1
            if self.param['typegraph'] != 1 :
                txt = 'Variables '
                if self.param['qui'] == 0 : value = 'actives'
                if self.param['qui'] == 1 : value = 'supplémentaires'
                if self.param['qui'] == 2 : value = 'étoilées'
                if self.param['qui'] == 3 : value = 'classes'
                txt += value + ' - '
                if self.param['what'] == 0 : value = 'Coordonnées'
                if self.param['what'] == 1 : value = 'Corrélations'
                txt += value + ' - facteur %i / %i' % (self.param['facteur'][0], self.param['facteur'][1])
                if self.param['do_select_nb'] : txt += ' - sélection de %i variables' % self.param['select_nb']
                if self.param['do_select_chi'] : txt += ' - sélection des variables avec chi2 > %i ' % self.param['select_chi']
                if self.param['over'] : txt += ' - Eviter les recouvrements'
                if self.param['cex_txt'] : txt += ' - taille du texte proportionnel à la masse'
                if self.param['tchi'] : txt += ' - taille du texte proportionnel au chi2 d\'association'
                #list_graph = read_list_file(self.DictPathOut[self.itempath], self.coding)
                if self.param['svg'] :
                    filename, ext = os.path.splitext(self.fileout)
                    self.fileout = filename + '.svg'
                if self.param['typegraph'] == 2 :
                    parametres = {'gexffile' :  self.fileout,
                                  'titre': 'Le titre',
                                  'nodemin': self.param['txt_min'],
                                  'nodemax': self.param['txt_max'],
                                  'bargraphw' : 60*int(self.param['clnb']),
                    }
                    web = WebExport(self.ira, parametres)
                    self.fileout = web.exportafc()
                if self.param['typegraph'] == 3 :
                    fileout = os.path.join(os.path.basename(self.fileout), 'index.html')
                else :
                    fileout = os.path.basename(self.fileout)
                self.list_graph.append([fileout, txt])
                print_liste(self.DictPathOut[self.itempath], self.list_graph)
                if self.param['svg'] or self.param['typegraph'] == 2:
                    self.listimg.append(hl.HyperLinkCtrl(self.panel_1, -1, self.fileout, URL=self.fileout))
                elif self.param['typegraph'] == 3 :
                    fileout = os.path.join(self.fileout,'index.html')
                    self.listimg.append(hl.HyperLinkCtrl(self.panel_1, -1, fileout, URL=fileout))
                else :
                    self.listimg.append(wx.StaticBitmap(self.panel_1, -1, wx.Bitmap(self.fileout, wx.BITMAP_TYPE_ANY), name=repr(len(self.list_graph) - 1)))
                    self.listimg[-1].Bind(wx.EVT_RIGHT_DOWN, self.onrightclick)
                self.sizer_3.Add( self.listimg[-1], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                self.labels.append(wx.StaticText(self.panel_1,-1, txt))
                self.sizer_3.Add(self.labels[-1], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                self.buts.append(wx.Button(self.panel_1, wx.ID_DELETE, name = repr(len(self.list_graph) - 1)))
                self.sizer_3.Add(self.buts[-1], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                self.sizer_3.Fit(self.panel_1)
                self.Layout()

                self.panel_1.Scroll(0,self.panel_1.GetScrollRange(wx.VERTICAL))
    #       elif self.param['typegraph'] == 2 :
    #           parametres = {'gexffile' :  self.fileout,
    #                           'titre': 'Le titre',
    #                           'nodemin': self.param['txt_min'],
    #                           'nodemax': self.param['txt_max'],
    #                           'bargraphw' : 60*int(self.param['clnb']),
    #           }
    #           web = WebExport(self.ira, parametres)
    #           afcout = web.exportafc()
    #           dial = SimpleDialog(self.ira)
    #           dial.link.SetLabel(afcout)
    #           dial.link.SetURL(afcout)
    #           dial.Layout()
    #           dial.ShowModal()


class GraphPanel(wx.ScrolledWindow):

    def __init__(self, parent, dico, list_graph, txt = '', style = wx.TAB_TRAVERSAL):
        wx.ScrolledWindow.__init__(self, parent, style = style)
        self.Dict = dico
        self.txt = txt
        self.parent = parent
        #self.SetFont(wx.Font(10, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, 0, "Arial"))
        self.labels = []
        self.listimg = []
        self.dirout = os.path.dirname(self.Dict['ira'])
        self.deb = wx.StaticText(self, -1, txt)
        for i in range(0,len(list_graph)):
            if os.path.exists(os.path.join(self.dirout,list_graph[i][0])) :
                filename, ext = os.path.splitext(list_graph[i][0])
                if ext == '.svg' :
                    self.listimg.append(hl.HyperLinkCtrl(self, -1, os.path.join(self.dirout,list_graph[i][0]), URL=os.path.join(self.dirout,list_graph[i][0])))
                else :
                    self.listimg.append(wx.StaticBitmap(self, -1, wx.Bitmap(os.path.join(self.dirout,list_graph[i][0]), wx.BITMAP_TYPE_ANY)))
                self.labels.append(wx.StaticText(self, -1, list_graph[i][1]))
        self.Bind(wx.EVT_MOTION, self.onMouseMove)
        self.__set_properties()
        self.__do_layout()

    def __set_properties(self):
        self.EnableScrolling(True,True)
        self.SetScrollRate(20, 20)
        self.SetFocus()

    def __do_layout(self):
        self.sizer_1 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_2 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_3 = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer_1.Add(self.deb)
        for i in range(0, len(self.listimg)):
            self.sizer_1.Add(self.listimg[i], 1, wx.ALIGN_CENTER_HORIZONTAL, 0)
            self.sizer_1.Add(self.labels[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.sizer_2.Add(self.sizer_1, 1, wx.EXPAND, 0)
        self.SetSizer(self.sizer_1)
        self.sizer_1.Fit(self)

    def onMouseMove(self, event):
        self.SetFocus()

def open_antiprofil(panel, AntiProfile, encoding, title = _("Antiprofiles"), translation = False, lems=None) :
    if not translation :
        DictAnti = ReadProfileAsDico(AntiProfile, True, encoding)
    else :
        DictAnti = AntiProfile
    panel.AntiProfNB = aui.AuiNotebook(panel, -1, wx.DefaultPosition)
    for i in range(0, panel.parametres['clnb']):
        tabantiprofile = ProfListctrlPanel(panel.parent, panel, DictAnti[str(i + 1)], True, i + 1, translation = translation)
        tabantiprofile.lems = lems
        panel.AntiProfNB.AddPage(tabantiprofile, 'classe %s' % str(i + 1))
    panel.TabChdSim.AddPage(panel.AntiProfNB, title)

def getlemgram(corpus, lem) :
    if not lem[6] in corpus.lems :
        return lem[5]
    else :
        return corpus.lems[lem[6]].gram


class OpenCHDS():

    def __init__(self, parent, corpus, parametres, Alceste=False):
        sep=' '
        self.parent = parent
        self.corpus = corpus
        self.parametres = parametres
        self.pathout = PathOut(parametres['ira'])
        self.pathout.basefiles(ChdTxtPathOut)
        DictPathOut = self.pathout
        self.DictPathOut = DictPathOut
        self.dictpathout = DictPathOut
        self.Alceste = Alceste
        Profile = DictPathOut['PROFILE_OUT']
        AntiProfile = DictPathOut['ANTIPRO_OUT']
#        self.encoding = self.parametres['encoding']
        if isinstance(self.corpus, Corpus) :
            self.corpus.make_ucecl_from_R(self.pathout['uce'])
            corpname = self.corpus.parametres['corpus_name']
        else :
            corpname = self.corpus.parametres['matrix_name']
            if os.path.exists(self.pathout['analyse.db']) :
                self.corpus.read_tableau(self.pathout['analyse.db'])
        clnb = parametres['clnb']
        dlg = progressbar(self, maxi = 4 + clnb)
        self.clnb = clnb
        print('lecture des profils')
        dlg.Update(2, _("Reading profiles"))
        DictProfile = ReadProfileAsDico(Profile, Alceste)
        self.DictProfile = DictProfile
        self.cluster_size = []
        clusternames = {}
        for i in range(0, clnb) :
            clusternames[i] = ' '.join(['%i' % (i + 1), _('Cluster'),  '%i' % (i + 1)])
        if os.path.exists(self.pathout['classes_names.txt']) :
            with open(self.pathout['classes_names.txt'], 'r', encoding='utf8') as f :
                clusternames_ = f.read()
            clusternames_ =  dict([[i, ' '.join([repr(i + 1), line])] for i, line in enumerate(clusternames_.splitlines())])
            clusternames.update(clusternames_)
        #DictAnti = ReadProfileAsDico(self, AntiProfile, Alceste, self.encoding)
        #
        # preparation de l'affichage
        #
        panel = wx.Panel(parent, -1)
        sizer1 = wx.BoxSizer(wx.VERTICAL)
        if os.path.exists(DictPathOut['pre_rapport']):
            with open(DictPathOut['pre_rapport'], 'r', encoding='utf8') as f :
                txt = f.read()
            self.debtext = txt
        else :
            self.debtext = ''
    #   panel.chd_toolbar = wx.ToolBar(panel, -1, wx.DefaultPosition, wx.DefaultSize, wx.TB_FLAT | wx.TB_NODIVIDER)
    #   panel.chd_toolbar.SetToolBitmapSize(wx.Size(16, 16))
        if isinstance(self.corpus, Corpus) :
            panel.corpus = self.corpus
        else :
            panel.tableau = self.corpus
            #self.parent.tableau = panel.tableau
        panel.dictpathout = self.DictPathOut
        panel.pathout = self.DictPathOut
        panel.parent = self.parent
        panel.DictProfile = self.DictProfile
        panel.cluster_size = self.cluster_size
        panel.debtext = self.debtext
    #   self.ID_rapport = wx.NewId()
    #   #rap_img = wx.Image(os.path.join(self.parent.images_path,'icone_rap_16.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
    #   #panel.chd_toolbar.AddLabelTool(self.ID_rapport, "rapport", rap_img, shortHelp=u"Produire le rapport", longHelp=u"Exporter un rapport en texte simple")
    #   butrap = wx.Button(panel.chd_toolbar, self.ID_rapport, u"Rapport ")
    #   panel.chd_toolbar.AddControl(butrap)
    #   panel.chd_toolbar.Realize()
    #   sizer1.Add(panel.chd_toolbar,0, wx.EXPAND, 5)
    #    self.TabChdSim = wx.aui.AuiNotebook(self.parent.nb, -1, wx.DefaultPosition)
        notebook_flags =  aui.AUI_NB_DEFAULT_STYLE | aui.AUI_NB_TAB_EXTERNAL_MOVE | aui.AUI_NB_TAB_MOVE | aui.AUI_NB_TAB_FLOAT| wx.NO_BORDER
        panel.TabChdSim = aui.AuiNotebook(panel, -1, wx.DefaultPosition)

       #panel.TabChdSim = LB.LabelBook(panel, -1, agwStyle = INB_TOP|INB_SHOW_ONLY_TEXT|INB_FIT_LABELTEXT)
        panel.TabChdSim.SetAGWWindowStyleFlag(notebook_flags)
        #panel.TabChdSim.SetArtProvider(aui.ChromeTabArt())
        #font = wx.Font(self.parent.fontsize, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
        #panel.TabChdSim.SetFont(font)
        sizer1.Add(panel.TabChdSim,10, wx.EXPAND, 5)
        panel.SetSizer(sizer1)
        sizer1.Fit(panel)
        if isinstance(self.corpus, Corpus) :
            panel.TabChdSim.corpus = corpus
            panel.TabChdSim.corpus.dictpathout = self.DictPathOut
        else :
            panel.TabChdSim.tableau = corpus
            panel.TabChdSim.tableau.dictpathout = self.DictPathOut
        panel.parametres = self.parametres
        self.panel = panel
        self.notenb = self.parent.nb.GetPageCount()
        if os.path.exists(self.DictPathOut['liste_graph_chd']) :
            list_graph = read_list_file(self.DictPathOut['liste_graph_chd'])
            CHD = GraphPanelDendro(panel.TabChdSim, DictPathOut, list_graph, txt = self.debtext)
            panel.TabChdSim.AddPage(CHD,'CHD')
    #    panel.ProfNB.SetArtProvider(aui.ChromeTabArt())
    #    panel.ProfNB = LB.LabelBook(panel, -1, agwStyle = INB_LEFT|INB_SHOW_ONLY_TEXT|INB_FIT_LABELTEXT)
    #    panel.ProfNB = wx.Listbook(self.parent, -1, style = wx.BK_DEFAULT)
    #    panel.ProfNB = wx.Treebook(self.parent, -1, style = wx.BK_DEFAULT)
    #    self.ProfNB.SetTabCtrlHeight(100)
    #    panel.AntiProfNB = aui.AuiNotebook(panel, -1, wx.DefaultPosition)
        if os.path.exists(DictPathOut['prof_seg']) :
            prof_seg = ReadProfileAsDico(DictPathOut['prof_seg'], False)
            self.prof_seg_nb = aui.AuiNotebook(panel, -1, wx.DefaultPosition)
        panel.ProfNB = aui.AuiNotebook(panel, -1, wx.DefaultPosition)
        notebook_flags |= aui.AUI_NB_WINDOWLIST_BUTTON
        panel.ProfNB.SetAGWWindowStyleFlag(notebook_flags)
        for i in range(0, clnb):
            self.cluster_size.append(DictProfile[str(i + 1)][0][0:3])
            if isinstance(self.corpus, Corpus) :
                DictProfile[str(i + 1)][1:] = [val[0:5] + [getlemgram(self.corpus, val)] + val[6:] for val in DictProfile[str(i + 1)][1:]]
            dlg.Update(3+i, 'Classe %i' %(i+1))
            ind = '/'.join(DictProfile[str(i + 1)][0][0:2]).strip()
            indpour = '\n'.join([ind, DictProfile[str(i + 1)][0][2]])
            self.tabprofile = ProfListctrlPanel(self.parent, self.panel, DictProfile[str(i + 1)], Alceste, i + 1)
            #self.tabantiprofile = ProfListctrlPanel(self.parent, self, DictAnti[str(i + 1)], Alceste, i + 1)
            panel.ProfNB.AddPage(self.tabprofile, clusternames[i] + '\n%s%%' % indpour, True)
            panel.ProfNB.SetPageTextColour(i, '#890909')
            panel.ProfNB.SetRenamable(i, True)
    #       panel.AntiProfNB.AddPage(self.tabantiprofile, 'classe %s' % str(i + 1))
            if os.path.exists(DictPathOut['prof_seg']) :
                self.tab_prof_seg = ProfListctrlPanel(self.parent, self, prof_seg[str(i + 1)], False, i + 1)
                self.prof_seg_nb.AddPage(self.tab_prof_seg, _("Cluster") + ' %i' % (i + 1))
        panel.ProfNB.SetSelection(0)
        #panel.ProfNB.SetFont(font)
        if clnb > 2 :
            self.TabAFC = aui.AuiNotebook(panel.TabChdSim, -1, wx.DefaultPosition)
            log.info('read AFC')
            list_graph=read_list_file(DictPathOut['liste_graph_afc'])
            self.tabAFCGraph = GraphPanelAfc(self.TabAFC, DictPathOut, list_graph, self.clnb)
            self.TabAFC.AddPage(self.tabAFCGraph, _("CA"))
            if os.path.exists(self.DictPathOut['afc_facteur']) :
                dictrow, first = ReadList(self.DictPathOut['afc_facteur'])
                self.TabAFC_facteur = ListForSpec(self.parent, parametres, dictrow, first[1:])
    #           dictrow, first = ReadList(self.DictPathOut['afc_row'], self.encoding)
    #           self.TabAFC_ligne = ListForSpec(self.parent, self.parametres, dictrow, first)
    #           dictrow, first = ReadList(self.DictPathOut['afc_col'], self.encoding)
    #           self.TabAFC_colonne = ListForSpec(parent, self.parametres, dictrow, first)
                self.TabAFC.AddPage(self.TabAFC_facteur, _("Factor"))
    #           self.TabAFC.AddPage(self.TabAFC_colonne, 'Colonnes')
    #           self.TabAFC.AddPage(self.TabAFC_ligne, 'Lignes')
            sizer_3 = wx.BoxSizer(wx.VERTICAL)
            self.parent.nb_panel_2 = wx.Panel(panel.TabChdSim, -1)
            self.parent.button_simi = wx.Button(self.parent.nb_panel_2, -1, "Voyager")
            self.parent.simi3dpanel = simi3d(self.parent.nb_panel_2, -1)
            sizer_3.Add(self.parent.simi3dpanel, 1, wx.EXPAND, 0)
            sizer_3.Add(self.parent.button_simi, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
            self.parent.nb_panel_2.SetSizer(sizer_3)
            self.TabAFC.AddPage(self.parent.nb_panel_2, _("3D graph"))
            self.parent.Bind(wx.EVT_BUTTON, self.onsimi, self.parent.button_simi)
        panel.TabChdSim.AddPage(panel.ProfNB, _("Profiles"))
    #   panel.TabChdSim.AddPage(panel.AntiProfNB, 'Antiprofils')
        dlg.Update(4 + self.clnb, 'Affichage...')
        if clnb > 2 :
            panel.TabChdSim.AddPage(self.TabAFC, _("CA"))
        if os.path.exists(DictPathOut['prof_seg']) :
            panel.TabChdSim.AddPage(self.prof_seg_nb, _("Repeated segments profiles"))
    #   panel.Bind(wx.EVT_BUTTON, self.ongetrapport, id = self.ID_rapport)
        if os.path.exists(os.path.join(self.parametres['pathout'], 'tgenchi2.csv')) :
            self.parametres['tgenspec'] = os.path.join(self.parametres['pathout'], 'tgenchi2.csv')
            TgenLayout(panel)
        if os.path.exists(self.dictpathout['translations.txt']) :
            with open(self.dictpathout['translations.txt'], 'r', encoding='utf8') as f:
                translist = f.read()
            translist = [line.split('\t') for line in translist.splitlines()]
            for line in translist :
                self.opentrans(line)
        panel.TabChdSim.SetSelection(0)
        self.parent.nb.AddPage(panel, _("Clustering") + ' - %s' % corpname)
        self.parent.ShowTab(True)
        self.parent.nb.SetSelection(self.parent.nb.GetPageCount() - 1)
    #   for pane in self.parent._mgr.GetAllPanes() :
    #       if isinstance(pane.window, aui.AuiNotebook):
    #           nb = pane.window
    #           nb.SetAGWWindowStyleFlag(notebook_flags)
    #           nb.SetArtProvider(aui.ChromeTabArt())
        dlg.Destroy()
        self.parent._mgr.Update()

    def opentrans(self, trans) :
        prof = ReadProfileAsDico(self.dictpathout[trans[0]], False)
        with open(self.dictpathout[trans[1]], 'r', encoding='utf8') as f :
            lems = f.read()
        lems = [line.split('\t') for line in lems.splitlines()]
        lems = dict(lems)
        open_antiprofil(self.panel, prof, 'utf8', title = trans[0], translation=True, lems=lems)
        self.panel.lems = lems
        self.panel.TabChdSim.SetSelection(self.panel.TabChdSim.GetPageCount() - 1)

    def onsimi(self,event):
        outfile = print_simi3d(self)
        error = exec_rcode(self.parent.RPath, outfile, wait = True)

    def onclusterstat(self, evt) :
        dial = PrefSimpleFile(self, self.parent, **{'mask' : '*.csv', 'title': 'Stat par classe'})
        dial.fbb.SetValue( os.path.join(os.path.dirname(self.corpus.dictpathout['ira']), 'stat_par_classe.csv'))
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            fileout = dial.fbb.GetValue()
            dial.Destroy()
            self.corpus.get_stat_by_cluster(fileout)
            msg = "Fini !"
            dlg = wx.MessageDialog(self.parent, msg, _("Stat by cluster"), wx.OK | wx.ICON_INFORMATION)
            dlg.CenterOnParent()
            if dlg.ShowModal() == wx.ID_OK :
                dlg.Destroy()

    #def onsearchf(self, evt) :
    #    if 'FrameSearch' not in dir(self.panel) :
    #        self.panel.FrameSearch = SearchFrame(self.parent, -1, "Rechercher...", self.corpus)
    #    self.panel.FrameSearch.Show()

def PrintRapport(self, corpus, parametres, istxt = True):
    sep = '\n'
    txt = """
+-+-+-+-+-+-+-+-+
|i|R|a|M|u|T|e|Q| - %s
+-+-+-+-+-+-+-+-+


""" % datetime.datetime.now().ctime()
    if istxt :
        totocc = corpus.gettotocc()
        txt += ': '.join([_('Number of texts'),  '%i%s' % (corpus.getucinb(), sep)])
        txt += ': '.join([_('Number of text segments'),  '%i%s' % (corpus.getucenb(), sep)])
        txt += ': '.join([_('Number of forms'), '%i%s' % (len(corpus.formes), sep)])
        txt += ': '.join([_('Number of occurrences'), '%i%s' % (totocc, sep)])
        #txt += 'moyenne d\'occurrences par forme: %f%s' % (float(totocc) / float(len(self.corpus.formes)), sep)
        txt += ': '.join([_('Number of lemmas'), '%i%s' % (len(corpus.lems), sep)])
        txt += ': '.join([_('Number of active forms'), '%i%s' % (corpus.getactivesnb(1), sep)])
        txt += ': '.join([_('Number of supplementary forms'), '%i%s' % (corpus.getactivesnb(2), sep)])
        txt += ' >= '.join([_('Number of active forms with a frequency'), '%i: %i%s' % (parametres['eff_min_forme'], parametres['nbactives'], sep)])
        txt += ': '.join([_('Mean of forms by segment'), '%f%s' % (float(totocc) / float(corpus.getucenb()), sep)])
        if 'tailleuc1' in parametres :
            if parametres['classif_mode'] == 0 :
                txt += ': '.join([_('Size of rst1 / rst2'), '%i / %i - %i / %i%s' % (parametres['tailleuc1'], parametres['tailleuc2'], parametres['lenuc1'], parametres['lenuc2'], sep)])
    else :
        self.Ucenb = self.nbind
        txt += ': '.join([_('Number of lines'), '%i%s' % (self.nbind, sep)])
        txt += ': '.join([_('Number of clusters'), '%i%s' % (self.clnb, sep)])
    if istxt :
        txt += ': '.join([_('Number of clusters'), '%i%s' % (parametres['clnb'], sep)])
        if parametres['classif_mode'] == 0 or parametres['classif_mode'] == 1 :
            txt += ' '.join(['%i' % sum([len(cl) for cl in corpus.lc]), _('segments classified on'), '%i (%.2f%%)%s' % (corpus.getucenb(), (float(sum([len(cl) for cl in corpus.lc])) / float(corpus.getucenb())) * 100, sep)])
        elif self.parametres['classif_mode'] == 2 :
            txt += ' '.join(['%i' % sum([len(cl) for cl in corpus.lc]), _('texts classified on'), '%i (%.2f%%)%s' % (corpus.getucinb(), (float(sum([len(cl) for cl in corpus.lc]))) / float(corpus.getucinb()) * 100, sep)])
    else :
        txt += ' '.join(['%i' % self.ucecla, _('line classified on'), '%i (%.2f%%)%s' % (self.Ucenb, (float(self.ucecla) / float(self.Ucenb)) * 100, sep)])

    txt += ''.join([sep, '###########################', sep, _('time'), ' : %s' % parametres.get('time', ''), sep, '###########################', sep])
    # ecriture du resultat dans le fichier
    with open(self.pathout['pre_rapport'], 'w', encoding='utf8') as f :
        f.write(txt)


class SashList(wx.Panel) :

    def __init__(self, parent) :
        wx.Panel.__init__(self, parent, -1)
        self.parent=parent
        winids = []
        #self.gparent=gparent
        #self.dlist=dlist
        #self.first = first
        #self.menu = menu
        # A window to the left of the client window
        #self.listlex = listlex
        self.leftwin1 =  wx.SashLayoutWindow(
                self, -1, wx.DefaultPosition, (200, 300),
                wx.NO_BORDER|wx.SW_3D
                )
        self.leftwin1.SetDefaultSize((120, 1000))
        self.leftwin1.SetOrientation(wx.LAYOUT_VERTICAL)
        self.leftwin1.SetAlignment(wx.LAYOUT_LEFT)
        self.leftwin1.SetBackgroundColour(wx.Colour(0, 255, 0))
        self.leftwin1.SetSashVisible(wx.SASH_RIGHT, True)
        self.leftwin1.SetExtraBorderSize(10)
        #textWindow = wx.TextCtrl(
        #                leftwin1, -1, "", wx.DefaultPosition, wx.DefaultSize,
        #                wx.TE_MULTILINE|wx.SUNKEN_BORDER
        #                )
        #textWindow.SetValue("A sub window")
        self.leftWindow1 = self.leftwin1
        winids.append(self.leftwin1.GetId())
        rightwin1 =  wx.SashLayoutWindow(
                self, -1, wx.DefaultPosition, (200, 300),
                wx.NO_BORDER|wx.SW_3D
                )
        rightwin1.SetDefaultSize((120, 1000))
        rightwin1.SetOrientation(wx.LAYOUT_VERTICAL)
        rightwin1.SetAlignment(wx.LAYOUT_LEFT)
        rightwin1.SetBackgroundColour(wx.Colour(0, 255, 0))
        rightwin1.SetSashVisible(wx.SASH_RIGHT, True)
        rightwin1.SetExtraBorderSize(10)
        #textWindow = wx.TextCtrl(
        #                leftwin1, -1, "", wx.DefaultPosition, wx.DefaultSize,
        #                wx.TE_MULTILINE|wx.SUNKEN_BORDER
        #                )
        #textWindow.SetValue("A sub window")
        self.rightwin1 = rightwin1
        winids.append(rightwin1.GetId())


class TgenLayout :

    def __init__(self, page):
        self.page = page
        parametres = self.page.parametres
        ira = wx.GetApp().GetTopWindow()
        tgenpath = os.path.join(parametres['pathout'], 'tgen.csv')
        self.page.tgens, etoiles =  ReadList(parametres['tgenspec'], 'utf8', sep="\t")
        tgen = TGen(path = tgenpath, encoding = 'UTF-8')
        tgen.read()
        tgenlempath = os.path.join(parametres['pathout'], 'tgenlemchi2.csv')
        if os.path.exists(tgenlempath) :
            self.page.parametres['tgenlemspec'] = tgenlempath
            self.page.tgenlem, etoiles = ReadList(self.page.parametres['tgenlemspec'], ira.syscoding, sep="\t")
        tgentab = False
        gparent = None
        if 'TabChdSim' in dir(page) :
            page = page.TabChdSim
        for i in range(page.GetPageCount()) :
            tab = page.GetPage(i)
            if 'gparent' in dir(tab) :
                if tab.gparent is not None :
                    gparent = tab.gparent
            if 'tgen' in dir(tab) :
                if tab.tgen :
                    tgentab = tab
                    break
        if tgentab :
            self.page.tgentab.RefreshData(self.page.tgens)
            self.page.tgentab.tgens = tgen.tgen
            self.page.tgentab.tgenlem = self.page.tgenlem
            page.SetSelection(i)
        else :
            self.page.tgentab = ListForSpec(ira, gparent, self.page.tgens, etoiles[1:])
            self.page.tgentab.tgen = True
            self.page.tgentab.tgens = tgen.tgen
            if os.path.exists(tgenlempath) :
                self.page.tgentab.tgenlem = self.page.tgenlem
            page.AddPage(self.page.tgentab, _('Tgens Specificities'))
            page.SetSelection(page.GetPageCount() - 1)


class dolexlayout :

    def __init__(self, ira, corpus, parametres):
        self.pathout = PathOut(dirout = parametres['pathout'])
        self.corpus = corpus
        self.dictpathout = StatTxtPathOut(parametres['pathout'])
        #self.corpus.read_corpus_from_shelves(self.corpus.dictpathout['db'])
        self.parent = ira
        self.corpus.parametres['syscoding'] = 'UTF8'
        self.encoding = self.corpus.parametres['syscoding']
        self.parametres = parametres
        self.DictSpec, first = ReadList(self.dictpathout['tablespecf'], self.corpus.parametres['syscoding'])
        if os.path.exists(self.pathout['banalites.csv']) :
            self.dictban, firstban = ReadList(self.pathout['banalites.csv'], self.corpus.parametres['syscoding'])
        if os.path.exists(self.pathout['statbyet.csv']) :
            self.dictstat, first = readliststat(self.pathout['statbyet.csv'])

        self.DictType, firstt = ReadList(self.dictpathout['tablespect'], self.corpus.parametres['syscoding'])
        self.DictEff, firsteff = ReadList(self.dictpathout['tableafcm'], self.corpus.parametres['syscoding'])
        self.DictEffType, firstefft = ReadList(self.dictpathout['tabletypem'], self.corpus.parametres['syscoding'])
        self.DictEffRelForme, firsteffrelf = ReadList(self.dictpathout['eff_relatif_forme'], self.corpus.parametres['syscoding'])
        self.DictEffRelType, firsteffrelt = ReadList(self.dictpathout['eff_relatif_type'], self.corpus.parametres['syscoding'])
        self.etoiles = firsteff[1:]
        #sash = SashList(ira.nb)
        self.TabStat = aui.AuiNotebook(ira.nb, -1, wx.DefaultPosition)
        self.TabStat.parametres = parametres
        #self.ListPan = LexPanel(self, ira, self.DictSpec, self.etoiles)
        self.ListPan = ListForSpec(ira, self, self.DictSpec, self.etoiles)
        self.ListPan.pathout = self.pathout
        if os.path.exists(self.pathout['banalites.csv']) :
            self.listban = ListForSpec(ira, self, self.dictban, ['eff'] + self.etoiles, usefirst = True)
        if os.path.exists(self.pathout['statbyet.csv']) :
            self.liststat =  ListForSpec(ira, self,self.dictstat, self.etoiles)
            self.liststat.pathout = self.pathout
        #self.ListPan2 = ListForSpec(sash.rightwin1, self, self.DictSpec, first)
        self.ListPant = ListForSpec(ira, self, self.DictType, self.etoiles)
        self.ListPant.pathout = self.pathout
        self.ListPanEff = ListForSpec(ira, self, self.DictEff, self.etoiles)
        self.ListPanEff.pathout = self.pathout
        self.ListPanEffType = ListForSpec(ira, self, self.DictEffType, self.etoiles)
        self.ListPanEffType.pathout = self.pathout
        self.ListPanEffRelForme = ListForSpec(ira, self, self.DictEffRelForme, self.etoiles)
        self.ListPanEffRelForme.pathout = self.pathout
        self.ListPanEffRelType = ListForSpec(ira, self, self.DictEffRelType, self.etoiles)
        self.ListPanEffRelType.pathout = self.pathout
        if os.path.exists(self.pathout['statbyet.csv']) :
            self.TabStat.AddPage(self.liststat, _('Statistics'))
        self.TabStat.AddPage(self.ListPan, _('Forms'))
        if os.path.exists(self.pathout['banalites.csv']) :
            self.TabStat.AddPage(self.listban, _('Banal forms'))
        self.TabStat.AddPage(self.ListPant, _('POS'))
        self.TabStat.AddPage(self.ListPanEff, _('Forms frequencies'))
        self.TabStat.AddPage(self.ListPanEffType, _('POS frequencies'))
        self.TabStat.AddPage(self.ListPanEffRelForme, _('Forms relative frequencies'))
        self.TabStat.AddPage(self.ListPanEffRelType, _('POS relative frequencies'))
        if self.parametres['clnb'] > 2 :
            self.TabAFC = aui.AuiNotebook(self.TabStat, -1, wx.DefaultPosition)
            list_graph=read_list_file(self.dictpathout['liste_graph_afcf'], encoding = self.encoding)
            self.tabAFCGraph = GraphPanelAfc(self.TabAFC, self.dictpathout, list_graph, self.parametres['clnb'], itempath ='liste_graph_afcf', coding = self.encoding, islex=True)
            self.TabAFC.AddPage(self.tabAFCGraph, _('CA forms'))
            list_graph=read_list_file(self.dictpathout['liste_graph_afct'], encoding = self.encoding)
            self.tabAFCTGraph = GraphPanelAfc(self.TabAFC, self.dictpathout, list_graph, self.parametres['clnb'], itempath ='liste_graph_afct', coding=self.encoding, islex=True)
            self.TabAFC.AddPage(self.tabAFCTGraph, _('CA POS'))
            self.TabStat.AddPage(self.TabAFC, _('CA'))
        ira.nb.AddPage(self.TabStat, ' - '.join([_('Specificities'), self.parametres['name']]))
        self.ira = ira
        self.TabStat.corpus = self.corpus
        self.TabStat.etoiles = self.etoiles
        self.TabStat.pathout = self.pathout
        if os.path.exists(os.path.join(self.parametres['pathout'], 'tgenspec.csv')) :
            self.parametres['tgenspec'] = os.path.join(self.parametres['pathout'], 'tgenspec.csv')
            TgenLayout(self.TabStat)
        self.TabStat.SetSelection(0)
        ira.nb.SetSelection(self.parent.nb.GetPageCount() - 1)
        ira.ShowAPane("Tab_content")


class StatLayout:

    def __init__(self, ira, corpus, parametres):
        self.pathout = PathOut(dirout = parametres['pathout'])
        self.corpus = corpus
        self.ira = ira
        self.read_result() # qui va définir la propriété self.result
        self.parametres = parametres
        self.TabStat = aui.AuiNotebook(ira.nb, -1, wx.DefaultPosition)
        self.TabStat.parametres = parametres
        self.TabStat.corpus = corpus
        self.TabStat.pathout = self.pathout
#        CHD = GraphPanel(panel.TabChdSim, DictPathOut, list_graph, txt = self.debtext)
#        panel.TabChdSim.AddPage(CHD,'CHD')
        #self.TabStatTot = wx.TextCtrl(self.TabStat, -1, style=wx.NO_BORDER | wx.TE_MULTILINE | wx.TE_RICH2)
        list_graph = [['zipf.png', 'zipf']]
        self.TabStatTot = GraphPanel(ira.nb, self.pathout, list_graph, self.result['glob'])
        self.TabStat.AddPage(self.TabStatTot, _('Abstract'))
        dictlabel = {'total' : _('Total'),
                     'formes_actives' : _('Actives forms'),
                     'formes_supplémentaires': _('Supplementary forms'),
                     'hapax' : _('Hapax'),
                     }
        for item in self.result:
            if item != 'glob':
                datam = [['forme', 'nb']] #???
                self.ListPan = ListPanel(ira, self, self.result[item])
                self.TabStat.AddPage(self.ListPan, dictlabel[item])
        ira.nb.AddPage(self.TabStat, '%s' % parametres['name'])
        ira.nb.SetSelection(ira.nb.GetPageCount() - 1)
        ira.ShowAPane("Tab_content")

    def read_result(self) :
        lcle = {'total' :'total.csv', 'formes_actives':'formes_actives.csv', 'formes_supplémentaires':'formes_supplémentaires.csv', 'hapax': 'hapax.csv'}
        self.result = {}
        for key in lcle :
            with open(self.pathout[lcle[key]], 'r', encoding='utf-8') as f :
                self.result[key] = [line.split(';') for line in f.read().splitlines()]
                self.result[key] = dict([[i,[line[0],int(line[1]), line[2]]] for i, line in enumerate(self.result[key])])
        with open(self.pathout['glob.txt'], 'r', encoding='utf-8') as f :
            self.result['glob'] = f.read()


class GraphPanelDendro(wx.Panel):

    def __init__(self,parent, dico, list_graph, txt=False):
        wx.Panel.__init__(self,parent)
        self.graphnb = 1
        self.dictpathout = dico
        self.dirout = os.path.dirname(self.dictpathout['ira'])
        self.list_graph = list_graph
        self.parent = self.GetParent()#parent
        self.tabchd = self.parent.GetParent()
        self.ira = self.tabchd.GetParent()
        self.SetFont(wx.Font(self.ira.fontsize, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, 0, "Arial")) #modifié
        self.labels = []
        self.listimg = []
        self.panel_1 = wx.ScrolledWindow(self, -1, style=wx.TAB_TRAVERSAL)
        self.panel_1.SetBackgroundColour('white')
        self.deb = wx.StaticText(self.panel_1, -1, txt)
        dendro_img = wx.Image(os.path.join(self.ira.images_path,'but_dendro.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        dendro_liste_img = wx.Image(os.path.join(self.ira.images_path,'but_dendro_liste.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        dendro_cloud_img= wx.Image(os.path.join(self.ira.images_path,'but_dendro_cloud.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        self.butdendro = wx.BitmapButton(self, -1, dendro_img)
        self.butdendrotexte = wx.BitmapButton(self, -1, dendro_liste_img)
        self.butdendrocloud = wx.BitmapButton(self, -1, dendro_cloud_img)
        for i in range(0,len(list_graph)):
            if os.path.exists(os.path.join(self.dirout,list_graph[i][0])) :
                filename, ext = os.path.splitext(list_graph[i][0])
                if ext == '.svg' :
                    self.listimg.append(hl.HyperLinkCtrl(self.panel_1, -1, os.path.join(self.dirout,list_graph[i][0]), URL=os.path.join(self.dirout,list_graph[i][0])))
                else :
                    self.listimg.append(wx.StaticBitmap(self.panel_1, -1, wx.Bitmap(os.path.join(self.dirout,list_graph[i][0]), wx.BITMAP_TYPE_ANY)))
                self.labels.append(wx.StaticText(self.panel_1, -1, list_graph[i][1]))
        self.__set_properties()
        self.__do_layout()

    def __set_properties(self):
        self.panel_1.EnableScrolling(True,True)
        #self.panel_1.SetSize((1000,1000))
        self.panel_1.SetScrollRate(20, 20)
        self.panel_1.SetFocus()
        self.Bind(wx.EVT_BUTTON, self.ondendro, self.butdendro)
        self.Bind(wx.EVT_BUTTON, self.ondendrotexte, self.butdendrotexte)
        self.Bind(wx.EVT_BUTTON, self.ondendrocloud, self.butdendrocloud)
        self.param = {'width' : 700,
                       'height': 500,
                       'type_dendro': 0,
                       'color_nb': 0,
                       'taille_classe' : True,
                       'type_tclasse' : 0,
                       'svg' : 0
                     }
        self.type_dendro = [ "phylogram", "cladogram", "fan", "unrooted", "radial" ]

    def __do_layout(self):
        self.sizer_1 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer_3 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_3.Add(self.deb)
        self.sizer_1.Add(self.butdendro, 0, 0, 0)
        self.sizer_1.Add(self.butdendrotexte, 0, 0, 0)
        self.sizer_1.Add(self.butdendrocloud, 0, 0, 0)
        for i in range(0, len(self.listimg)):
            self.sizer_3.Add(self.listimg[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
            self.sizer_3.Add(self.labels[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.panel_1.SetSizer(self.sizer_3)
        self.sizer_2.Add(self.sizer_1, 0, wx.EXPAND, 0)
        self.sizer_2.Add(self.panel_1, 1, wx.EXPAND, 0)
        self.SetSizer(self.sizer_2)

    def make_param(self, dial):
        self.param['width'] = dial.m_spinCtrl2.GetValue()
        self.param['height'] = dial.m_spinCtrl1.GetValue()
        self.param['type_dendro'] = dial.m_choice1.GetSelection()
        self.param['svg'] = dial.choice_format.GetSelection()
        if self.param['typedendro'] == 'classique' :
            self.param['color_nb'] = dial.m_radioBox1.GetSelection()
            self.param['taille_classe'] = dial.m_checkBox1.GetValue()
            self.param['type_tclasse'] = dial.m_radioBox2.GetSelection()
        if self.param.get('translation', False) :
            if dial.trans.GetSelection() == 0 :
                del self.param['translation']
            else :
                self.param['translation'] = self.param['translation'][dial.trans.GetSelection()-1][1]

    def make_dendro(self, dendro = 'simple') :
        if self.param['svg'] :
            typefile = '.svg'
        else :
            typefile = '.png'
        while os.path.exists(os.path.join(self.dirout, 'dendrogramme_' + str(self.graphnb)+typefile)) :
            self.graphnb += 1
        fileout = ffr(os.path.join(self.dirout,'dendrogramme_' + str(self.graphnb)+typefile))
        width = self.param['width']
        height = self.param['height']
        type_dendro = self.type_dendro[self.param['type_dendro']]
        if self.param['taille_classe'] :
            tclasse = 'TRUE'
        else :
            tclasse = 'FALSE'
        if self.param['color_nb'] == 0 :
            bw = 'FALSE'
        else :
            bw = 'TRUE'
        if self.param['type_tclasse'] == 0 :
            histo='FALSE'
        else :
            histo = 'TRUE'
        if self.param['svg'] :
            svg = 'TRUE'
        else :
            svg = 'FALSE'
        dendro_path = self.dictpathout['Rdendro']
        classe_path = self.dictpathout['uce']
        txt = """
        library(ape)
        load("%s")
        source("%s")
        classes <- read.csv2("%s", row.names=1)
        classes <- classes[,1]
        """ % (ffr(dendro_path), ffr(self.ira.RscriptsPath['Rgraph']),  ffr(classe_path))
        if dendro == 'simple' :
            txt += """
            open_file_graph("%s", width=%i, height=%i, svg=%s)
            plot.dendropr(tree.cut1$tree.cl, classes, type.dendro="%s", histo=%s, bw=%s, lab=NULL, tclasse=%s)
            """ % (ffr(fileout), width, height, svg, type_dendro, histo, bw, tclasse)
        elif dendro == 'texte' :
            txt += """
            load("%s")
            source("%s")
            if (is.null(debsup)) {
                debsup <- debet
            }
            chistable <- chistabletot[1:(debsup-1),]
            """ % (ffr(self.dictpathout['RData.RData']), ffr(self.ira.RscriptsPath['Rgraph']))
            if self.param.get('translation', False) :
                txt += """
                rn <- read.csv2("%s", header=FALSE, sep='\t')
                rnchis <- row.names(chistable)
                commun <- intersect(rnchis, unique(rn[,2]))
                idrnchis <- sapply(commun, function(x) {which(rnchis==x)})
                idrn <- sapply(commun, function(x) {which(as.vector(rn[,2])==x)[1]})
                rownames(chistable)[idrnchis] <- as.vector(rn[idrn,1])
                """ % ffr(self.param['translation'])
            txt += """
            open_file_graph("%s", width=%i, height=%i, svg = %s)
            plot.dendro.prof(tree.cut1$tree.cl, classes, chistable, nbbycl = 60, type.dendro="%s", bw=%s, lab=NULL)
            """ % (ffr(fileout), width, height, svg, type_dendro, bw)
        elif dendro == 'cloud' :
            txt += """
            load("%s")
            source("%s")
            if (is.null(debsup)) {
                debsup <- debet
            }
            chistable <- chistabletot[1:(debsup-1),]
            open_file_graph("%s", width=%i, height=%i, svg=%s)
            plot.dendro.cloud(tree.cut1$tree.cl, classes, chistable, nbbycl = 300, type.dendro="%s", bw=%s, lab=NULL)
            """ % (ffr(self.dictpathout['RData.RData']), ffr(self.ira.RscriptsPath['Rgraph']), ffr(fileout), width, height, svg, type_dendro, bw)
        tmpfile = tempfile.mktemp()
        # ecriture du fichier de script à éxécuter
        with open(tmpfile, 'w', encoding='utf8') as f :
            f.write(txt)
        # dialogue d'attente
        busy = wx.BusyInfo(_("Please wait..."), self.parent)
        wx.SafeYield()
        error = exec_rcode(self.ira.RPath, tmpfile, wait=True)
        del busy
        # fin de l'attente
        check_Rresult(self.ira, error)
        self.list_graph.append([fileout, 'Dendrogramme CHD1 - %s' %  type_dendro])
        print_liste(self.dictpathout['liste_graph_chd'], self.list_graph)
        if self.param['svg'] :
            self.sizer_3.Add(hl.HyperLinkCtrl(self.panel_1, -1, fileout, URL=fileout), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        else :
            self.sizer_3.Add(wx.StaticBitmap(self.panel_1, -1, wx.Bitmap(fileout, wx.BITMAP_TYPE_ANY)), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.sizer_3.Add(wx.StaticText(self.panel_1,-1, 'Dendrogramme CHD1 - %s' %  type_dendro), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.sizer_3.Fit(self.panel_1)
        self.Layout()
        self.panel_1.Scroll(0,self.panel_1.GetScrollRange(wx.VERTICAL))


    def ondendro(self, evt):
        self.param['typedendro'] = 'classique'
        dial = PrefDendro(self.ira, self.param)
        val = dial.ShowModal()
        if val == wx.ID_OK :
            self.make_param(dial)
            self.make_dendro()

    def ondendrotexte(self, evt):
        self.param['typedendro'] = 'texte'
        if os.path.exists(self.dictpathout['translations.txt']) :
            with codecs.open(self.dictpathout['translations.txt'], 'r', 'utf8') as f :
                content = f.read()
            print(content)
            trans = [line.split('\t')[1] for line in content.splitlines()]
            trans = [[val, self.dictpathout[val]] for val in trans]
            self.param['translation'] = trans
        dial = PrefDendro(self.ira, self.param)
        val = dial.ShowModal()
        if val == wx.ID_OK :
            self.make_param(dial)
            self.make_dendro(dendro = 'texte')

    def ondendrocloud(self, evt):
        self.param['typedendro'] = 'cloud'
        dial = PrefDendro(self.ira, self.param)
        val = dial.ShowModal()
        if val == wx.ID_OK :
            self.make_param(dial)
            self.make_dendro(dendro = 'cloud')


class OpenCorpus :

    def __init__(self, ira, parametres) :
        #self.text = wx.TextCtrl(ira, -1, "", wx.Point(0, 0), wx.Size(200, 200), wx.NO_BORDER | wx.TE_MULTILINE | wx.TE_RICH2 | wx.TE_READONLY)
        self.panel = CopusPanel(ira, parametres)
        ira.nb.AddPage(self.panel, 'Description %s' % parametres['corpus_name'])
        #self.text.write(DoConf().totext(parametres))
        ira.nb.SetSelection(ira.nb.GetPageCount() - 1)
        ira.ShowAPane("Tab_content")

class MatLayout :

    def __init__(self, ira, matrix):
        #self.parent.content = self.csvtable
        self.sheet = MySheet(ira.nb)
        ira.nb.AddPage(self.sheet, matrix.parametres['matrix_name'])
        self.sheet.Populate(matrix.csvtable)
        self.sheet.parametres = matrix.parametres
        ira.nb.SetSelection(ira.nb.GetPageCount() - 1)
        ira.ShowAPane("Tab_content")


class CopusPanel(wx.Panel) :

    def __init__(self, parent, parametres) :
        wx.Panel.__init__ ( self, parent, id = wx.ID_ANY, pos = wx.DefaultPosition, size = wx.Size( 500,300 ), style = wx.TAB_TRAVERSAL )
        self.parametres = parametres
        fgSizer5 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer5.SetFlexibleDirection( wx.BOTH )
        fgSizer5.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
        self.fgSizer5 = fgSizer5
        self.m_staticText18 = wx.StaticText( self, wx.ID_ANY, _("Description of corpus"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText18.Wrap( -1 )
        fgSizer5.Add( self.m_staticText18, 0, wx.ALL, 5 )
        self.m_staticText19 = wx.StaticText( self, wx.ID_ANY, "", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText19.Wrap( -1 )
        fgSizer5.Add( self.m_staticText19, 0, wx.ALL, 5 )
        self.m_staticText20 = wx.StaticText( self, wx.ID_ANY, "Nom", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText20.Wrap( -1 )
        fgSizer5.Add( self.m_staticText20, 0, wx.ALL, 5 )
        self.m_staticText21 = wx.StaticText( self, wx.ID_ANY, parametres['corpus_name'], wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText21.Wrap( -1 )
        fgSizer5.Add( self.m_staticText21, 0, wx.ALL, 5 )
        description = {'lang' : _('Language'),
                       'encoding' : _('Characters set'),
                       'ucinb' : _('Number of texts'),
                       'ucenb' : _('Number of text segments'),
                       'formesnb' : _('Number of forms'),
                       'hapax' : _('Number of hapax'),
                      }
        keys = ['lang', 'encoding', 'originalpath', 'pathout', 'date', 'time']
        self.addkeys(keys, description)
        self.m_staticText18 = wx.StaticText( self, wx.ID_ANY, "Paramètres", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText18.Wrap( -1 )
        fgSizer5.Add( self.m_staticText18, 0, wx.ALL, 5 )
        self.m_staticText19 = wx.StaticText( self, wx.ID_ANY, "", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText19.Wrap( -1 )
        fgSizer5.Add( self.m_staticText19, 0, wx.ALL, 5 )
        keys = ['ucemethod', 'ucesize', 'keep_caract', 'expressions']
        self.addkeys(keys, description)
        self.m_staticText18 = wx.StaticText( self, wx.ID_ANY, "Statistiques", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText18.Wrap( -1 )
        fgSizer5.Add( self.m_staticText18, 0, wx.ALL, 5 )
        self.m_staticText19 = wx.StaticText( self, wx.ID_ANY, "", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText19.Wrap( -1 )
        fgSizer5.Add( self.m_staticText19, 0, wx.ALL, 5 )
        keys = ['ucinb', 'ucenb', 'occurrences', 'formesnb', 'hapax']
        self.addkeys(keys, description)
        self.SetSizer( fgSizer5 )
        self.Layout()

    def addkeys(self, keys, description) :
        for key in keys :
            option = self.parametres.get(key,'non défini')
            if isinstance(option, int) :
                option = repr(option)
            text = wx.StaticText( self, wx.ID_ANY, description.get(key, key), wx.DefaultPosition, wx.DefaultSize, 0 )
            text.Wrap( -1 )
            self.fgSizer5.Add( text, 0, wx.ALL, 5 )
            text = wx.StaticText( self, wx.ID_ANY, option, wx.DefaultPosition, wx.DefaultSize, 0 )
            text.Wrap( -1 )
            self.fgSizer5.Add( text, 0, wx.ALL, 5 )


class DefaultTextLayout :

    def __init__(self, ira, corpus, parametres, cmd = False) :
        self.pathout = PathOut(dirout = parametres['pathout'])
        self.ira = ira
        self.parent = ira
        self.parametres = parametres
        self.corpus = corpus
        self.cmd = cmd
        self.dolayout()

    def dolayout(self, cmd) :
        log.info('no layout yet')


class WordCloudLayout(DefaultTextLayout):

    def dolayout(self):
        self.pathout.basefiles(simipath)
        self.Tab = aui.AuiNotebook(self.ira.nb, -1, wx.DefaultPosition)
        if self.parametres['svg'] :
            list_graph = [['nuage_1.svg', 'Nuage']]
        else :
            list_graph = [['nuage_1.png', 'Nuage']]
        self.TabStatTot = GraphPanel(self.ira.nb, self.pathout, list_graph)
        self.Tab.AddPage(self.TabStatTot, 'Nuage')
        self.Tab.corpus = self.corpus
        self.Tab.parametres = self.parametres
        self.ira.nb.AddPage(self.Tab, '%s' % self.parametres['name'])
        self.ira.nb.SetSelection(self.ira.nb.GetPageCount() - 1)
        self.ira.ShowAPane("Tab_content")


class LabbeLayout(DefaultTextLayout):

    def dolayout(self):
        self.Tab = aui.AuiNotebook(self.ira.nb, -1, wx.DefaultPosition)
        #if self.parametres['svg'] :
        #    list_graph = [['nuage_1.svg', 'Nuage']]
        #else :
        #    list_graph = [['nuage_1.png', 'Nuage']]
        list_graph = [['labbe-tree.png', _('Ward clustering (method ward2)')],
                     ['labbe-heatmap.png', _('Heatmap')],
                     ['labbe-matrix.png', _('Matrix')]]
        for val in list_graph :
            #self.TabStatTot = GraphPanel(self.ira.nb, self.pathout, [val])
            self.Tab.AddPage(GraphPanel(self.Tab, self.pathout, [val]), val[1])
        if os.path.exists(self.pathout['listdist.csv']) :
            self.dictlist, first = read_dist_list(self.pathout['listdist.csv'])
            self.listpan = ListPanel(self.ira, self, self.dictlist, context='labbe')
            self.Tab.AddPage(self.listpan, _("List"))
        self.Tab.corpus = self.corpus
        self.Tab.parametres = self.parametres
        self.ira.nb.AddPage(self.Tab, '%s' % self.parametres['name'])
        self.ira.nb.SetSelection(self.ira.nb.GetPageCount() - 1)
        self.ira.ShowAPane("Tab_content")


def blender(self):
    nodesfile = self.pathout['nodes.csv']
    edgesfile = self.pathout['edges.csv']
    jsonout = self.pathout.makenew('graphe_json', 'json')
    txt = """
    library(igraph)
    load("%s")
    source("%s")
    """ % (ffr(self.pathout['RData.RData']), ffr(self.parent.RscriptsPath['Rgraph']))
    txt += """
    nodesf <- "%s"
    edgesf <- "%s"
    """ % (ffr(nodesfile), ffr(edgesfile))
    txt += """
    if ("communities" %in% names(graph.simi)) {
        community = TRUE
    } else {
        community = FALSE
    }
    graph.to.file(graph.simi, nodesfile = nodesf, edgesfile = edgesf, community = community)
    """
    # ecriture du fichier de script à éxécuter
    filetmp = tempfile.mktemp()
    with open(filetmp, 'w', encoding='utf8') as f :
        f.write(txt)
    exec_rcode(self.ira.RPath, filetmp)
    GraphToJson(nodesfile, edgesfile, jsonout)
    # une fonction à ré-activer ???
    # pour le moment, j'ai mis le module network_to_blender de coté
    # launchcommand(['/home/pierre/prog/blender-2.73-linux-glibc211-x86_64/blender', '-P', os.path.join(self.ira.AppliPath, 'network_to_blender.py'), jsonout])


class SimiLayout(DefaultTextLayout) :

    def dolayout(self) :
        self.pathout.basefiles(simipath)
        self.actives = None
        self.indices = indices_simi
        if os.path.exists(self.pathout['liste_graph']) :
            list_graph = read_list_file(self.pathout['liste_graph'])
        else :
            list_graph = [['','']]
        if not self.cmd :
            notebook_flags =  aui.AUI_NB_DEFAULT_STYLE | aui.AUI_NB_TAB_EXTERNAL_MOVE | aui.AUI_NB_TAB_MOVE | aui.AUI_NB_TAB_FLOAT
            self.tabsimi = aui.AuiNotebook(self.ira.nb, -1, wx.DefaultPosition)
            self.tabsimi.SetAGWWindowStyleFlag(notebook_flags)
            self.tabsimi.SetArtProvider(aui.ChromeTabArt())
            self.tabsimi.corpus = self.corpus
            self.tabsimi.parametres = self.parametres
            self.graphpan = GraphPanelSimi(self.tabsimi, self.pathout, list_graph)
            self.graphpan.Bind(wx.EVT_BUTTON, self.redosimi, self.graphpan.butafc)
            self.graphpan.Bind(wx.EVT_BUTTON, self.export, self.graphpan.butexport)
            self.graphpan.Bind(wx.EVT_BUTTON, self.exportmat, self.graphpan.butexportmat)
            #self.graphpan.Bind(wx.EVT_BUTTON, self.blender, self.graphpan.butblender)
            self.tabsimi.AddPage(self.graphpan, _('Graph'))
            self.ira.nb.AddPage(self.tabsimi, _('Graph analysis'))
            self.ira.ShowTab(True)
            self.ira.nb.SetSelection(self.ira.nb.GetPageCount() - 1)

    def redosimi(self, evt) :
        redosimi(self, evt)
   #      with open(self.pathout['selected.csv'],'r') as f :
   #          selected = f.read()
   #      selected = [int(val) for val in selected.splitlines()]
   #      if self.actives is None :
   #          with codecs.open(self.pathout['actives.csv'], 'r', self.parametres['encoding']) as f :
   #              self.actives = f.read()
   #          self.actives = self.actives.splitlines()#[act for act in self.actives.splitlines()]
   #      if os.path.exists(self.pathout['actives_nb.csv']) :
   #          with open(self.pathout['actives_nb.csv'], 'r') as f :
   #              act_nb = f.read()
   #              act_nb = act_nb.splitlines()
   #          dictcol = dict([[i, [self.actives[i], int(act_nb[i])]] for i, val in enumerate(self.actives)])
   #      else :
   #          dictcol = dict([[i, [act, self.corpus.getlemeff(act)]] for i, act in enumerate(self.actives)])
   #      #res = SelectColumn(self.ira, dictcol, self.actives, self.pathout['selected.csv'], selected = selected, dlg = True)
   #      #if res.ok :
   #      prep = PrepSimi(self.ira, self, self.parametres,self.pathout['selected.csv'], self.actives, indices_simi, wordlist = dictcol, selected = selected)
   #      if prep.val == wx.ID_OK :
   #          self.parametres = prep.parametres
   #          script = PrintSimiScript(self)
   #          script.make_script()
   #          pid = exec_rcode(self.ira.RPath, script.scriptout, wait = True)
   #          check_Rresult(self.ira, pid)
   #          if self.parametres['type_graph'] in [1,3] :
   #              if self.parametres['svg'] :
   #                  filename, ext = os.path.splitext(script.filename)
   #                  fileout = filename + '.svg'
   #              elif self.parametres['type_graph'] == 3 :
   #                  fileout = script.filename
   #                  parametres = {'gexffile' :  fileout,
   #                                'dirout' : os.path.dirname(fileout),
   #                                'titre': 'Le titre',
   #                                #'nodemin': self.param['txt_min'],
   #                                #'nodemax': self.param['txt_max'],
   #                                #'bargraphw' : 60*int(self.param['clnb']),
   #                  }
   #                  web = WebExport(self.ira, parametres)
   #                  fileout = web.exportsimi()
   #              else :
   #                  fileout = script.filename
   #              if os.path.exists(self.pathout['liste_graph']):
   #                  graph_simi = read_list_file(self.pathout['liste_graph'])
   #                  graph_simi.append([os.path.basename(fileout), script.txtgraph])
   #              else :
   #                  graph_simi = [[os.path.basename(fileout), script.txtgraph]]
   #              print_liste(self.pathout['liste_graph'], graph_simi)
   #          DoConf().makeoptions([self.parametres['type']], [self.parametres], self.pathout['Analyse.ira'])
   #          if self.parametres['type_graph'] in [1,3] :
   #              if self.parametres['svg'] or self.parametres['type_graph'] == 3 :
   #                  self.graphpan.sizer_3.Add(hl.HyperLinkCtrl(self.graphpan.panel_1, -1, fileout, URL = fileout), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
   #              else :
   #                  self.graphpan.sizer_3.Add(wx.StaticBitmap(self.graphpan.panel_1, -1, wx.Bitmap(fileout, wx.BITMAP_TYPE_ANY)), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
   #              self.graphpan.sizer_3.Add(wx.StaticText(self.graphpan.panel_1,-1, script.txtgraph), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
   #              self.graphpan.sizer_3.Fit(self.graphpan.panel_1)
   #              self.graphpan.Layout()
   #              self.graphpan.panel_1.Scroll(0,self.graphpan.panel_1.GetScrollRange(wx.VERTICAL))

    def export(self, evt) :
        nb = 1
        while os.path.exists(os.path.join(self.pathout.dirout,'graph_'+str(nb)+'.graphml')):
            nb +=1
        fileout = ffr(os.path.join(self.pathout.dirout,'graph_'+str(nb)+'.graphml'))
        txt = """
        library(igraph)
        load("%s")
        source("%s")
        fileout <- "%s"
        graph <- graph.simi$graph
        V(graph)$x <- graph.simi$layout[,1]
        V(graph)$y <- graph.simi$layout[,2]
        if (length(graph.simi$label.cex == 1)) {
            V(graph)$weight <- graph.simi$eff
        } else {
            V(graph)$weight <- graph.simi$label.cex
        }
        V(graph)$rcolor <- vertex.label.color
        V(graph)$frequences <- graph.simi$mat.eff
        V(graph)$label <- as.character(graph.simi$v.label)
        E(graph)$weight <- graph.simi$we.width
        write.graph(graph, fileout, format = 'graphml')
        #saveAsGEXF(graph, filepath = fileout)
        """ % (ffr(self.pathout['RData.RData']), ffr(self.parent.RscriptsPath['simi']), fileout)
        filetmp = tempfile.mktemp()
        with open(filetmp, 'w', encoding='utf8') as f :
            f.write(txt)
        exec_rcode(self.ira.RPath, filetmp)
        mss = wx.MessageDialog(self.ira, fileout, _('File exported'), wx.OK)
        mss.CenterOnParent()
        mss.ShowModal()
        mss.Destroy()

    def blender(self, evt):
        blender(self)

    def exportmat(self, evt) :
        fileout = self.pathout['simimat.csv']
        txt = """
        load("%s")
        write.csv2(x$mat, "%s")
        """ % (ffr(self.pathout['RData.RData']), ffr(fileout))
        filetmp = tempfile.mktemp()
        with open(filetmp, 'w', encoding='utf8') as f :
            f.write(txt)
        exec_rcode(self.ira.RPath, filetmp)
        mss = wx.MessageDialog(self.ira, fileout, _('File exported'), wx.OK)
        mss.CenterOnParent()
        mss.ShowModal()
        mss.Destroy()



class DefaultMatLayout :

    def __init__(self, parent, tableau, parametres) :
        self.pathout = PathOut(dirout = parametres['pathout'])
        self.ira = parent
        self.parent = parent
        self.tableau = tableau
        self.parametres = parametres
        if os.path.exists(self.pathout['analyse.db']) :
            self.tableau.read_tableau(self.pathout['analyse.db'])
        self.dolayout()
        self.ira.nb.SetSelection(self.ira.nb.GetPageCount() - 1)
        self.ira.ShowAPane("Tab_content")

    def dolayout(self) :
        pass


class FreqLayout(DefaultMatLayout) :

    def dolayout(self) :
        self.tab = wx.html.HtmlWindow(self.ira.nb, -1)
        #self.tab = wx.html2.WebView.New(self)
        res = normpath_win32(self.pathout['resultats.html']).replace('\\','/')
        #self.tab.LoadPage(res)
        self.tab.LoadFile(res)
        #self.tab.LoadURL(res)
        self.tab.parametres = self.parametres
        self.ira.nb.AddPage(self.tab, ' - '.join([_("Frequency"), self.parametres['name']]))


class Chi2Layout(DefaultMatLayout) :

    def dolayout(self):
        self.tab = wx.html.HtmlWindow(self.ira.nb, -1)
        if "gtk2" in wx.PlatformInfo:
            self.tab.SetStandardFonts()
        res = normpath_win32(self.pathout['resultats-chi2.html']).replace('\\','/')
        self.tab.LoadFile(res)
        self.tab.parametres = self.parametres
        self.ira.nb.AddPage(self.tab, ' - '.join(["Chi2", self.parametres['name']]))
        #self.ira.nb.SetSelection(self.ira.nb.GetPageCount() - 1)
        #self.ira.ShowAPane("Tab_content")


class ProtoLayout(DefaultMatLayout) :

    def dolayout(self) :
        list_graph = [['proto.png', _('Prototypical analysis')]]
        #self.Tab = aui.AuiNotebook(self.ira.nb, -1, wx.DefaultPosition)
        #if self.parametres['svg'] :
        #    list_graph = [['nuage_1.svg', 'Nuage']]
        #else :
        #    list_graph = [['nuage_1.png', 'Nuage']]
        self.TabProto = GraphPanel(self.ira.nb, self.pathout, list_graph)
        #self.Tab.AddPage(self.TabProto, 'Analyse Prototypique')
        #self.Tab.corpus = self.corpus
        self.TabProto.parametres = self.parametres
        self.ira.nb.AddPage(self.TabProto, ' - '.join([_('Prototypical analysis'), self.parametres['name']]))
        #self.ira.nb.SetSelection(self.ira.nb.GetPageCount() - 1)
        #self.ira.ShowAPane("Tab_content")

class CateLayout(DefaultMatLayout) :

    def dolayout(self) :
        self.tableau.read_tableau(self.pathout['analyse.db'])
        TabCate = ElCategorizator(self.ira.nb, self.pathout, self.tableau)
        TabCate.parametres = self.parametres
        self.ira.nb.AddPage(TabCate, ' - '.join([_('ElCaTeGoRiZaToR'), self.parametres['name']]))

class SimiMatLayout(DefaultMatLayout) :

    def dolayout(self):
        self.pathout.basefiles(simipath)
        self.indices = indices_simi
        if os.path.exists(self.pathout['liste_graph']) :
            list_graph = read_list_file(self.pathout['liste_graph'])
        else :
            list_graph = [['','']]
        notebook_flags =  aui.AUI_NB_DEFAULT_STYLE | aui.AUI_NB_TAB_EXTERNAL_MOVE | aui.AUI_NB_TAB_MOVE | aui.AUI_NB_TAB_FLOAT
        self.tabsimi = aui.AuiNotebook(self.parent.nb, -1, wx.DefaultPosition)
        self.tabsimi.SetAGWWindowStyleFlag(notebook_flags)
        self.tabsimi.SetArtProvider(aui.ChromeTabArt())
        self.graphpan = GraphPanelSimi(self.tabsimi, self.pathout, list_graph)
        self.graphpan.Bind(wx.EVT_BUTTON, self.redosimi, self.graphpan.butafc)
        self.graphpan.Bind(wx.EVT_BUTTON, self.export, self.graphpan.butexport)
        #self.graphpan.Bind(wx.EVT_BUTTON, self.blender, self.graphpan.butblender)
        self.tabsimi.AddPage(self.graphpan, _('Graph'))
        self.tabsimi.parametres = self.parametres
        self.parent.nb.AddPage(self.tabsimi, ' - '.join([_('Graph analysis'), self.parametres['name']]))
        #self.parent.ShowTab(True)
        #self.parent.nb.SetSelection(self.parent.nb.GetPageCount() - 1)

    def redosimi(self,evt) :
        with open(self.pathout['selected.csv'],'r', encoding='utf8') as f :
            selected = f.read()
        selected = [int(val) for val in selected.splitlines()]
        #if self.actives is None :
        #    with codecs.open(self.pathout['actives.csv'], 'r', self.parametres['encoding']) as f :
        #        self.actives = f.read()
        #    self.actives = self.actives.splitlines()#[act for act in self.actives.splitlines()]
        try :
            actives = [[val, self.tableau.actives[val][0]] for val in self.tableau.actives]
        except :
            actives = [[val, self.tableau.actives[val]] for val in self.tableau.actives]
        #self.tableau.make_listactives()
        actives = dict([[i, val] for i, val in enumerate(actives)])
        print(actives)
        #dictcol = dict([[i, [act, self.corpus.getlemeff(act)]] for i, act in enumerate(self.actives)])
        self.dial = PrefSimi(self.parent, -1, self.parametres, self.indices, wordlist = actives, selected = selected, actives = self.tableau.listactives)
        self.dial.CenterOnParent()
        self.val = self.dial.ShowModal()
        if self.val == wx.ID_OK :
            last = self.dial.listcol.GetFirstSelected()
            lastl = [self.dial.listcol.GetFirstSelected()]
            indexes = [self.dial.listcol.getColumnText(self.dial.listcol.GetFirstSelected(),0)]
            while self.dial.listcol.GetNextSelected(last) != -1:
                last = self.dial.listcol.GetNextSelected(last)
                lastl.append(last)
                indexes.append(self.dial.listcol.getColumnText(last,0))
            self.column = [self.tableau.listactives.index(val) for val in indexes]
            self.column.sort()
            with open(self.pathout['selected.csv'], 'w', encoding='utf8') as f :
                f.write('\n'.join([repr(val) for val in self.column]))
            self.make_param()
            self.dial.Destroy()
            self.script = PrintSimiScript(self)
            self.script.make_script()
            self.tmpfile = self.script.scriptout
            dlg = progressbar(self, maxi = 2)
            self.DoR(dlg)
            dlg.Destroy()
            if self.parametres['type_graph'] == 1:
                if self.parametres['svg'] :
                    filename, ext = os.path.splitext(self.script.filename)
                    fileout = filename + '.svg'
                else :
                    fileout = self.script.filename
                fileout = normpath_win32(fileout)
                if os.path.exists(self.pathout['liste_graph']):
                    graph_simi = read_list_file(self.pathout['liste_graph'])
                    graph_simi.append([os.path.basename(fileout), self.script.txtgraph])
                else :
                    graph_simi = [[os.path.basename(fileout), self.script.txtgraph]]
                print_liste(self.pathout['liste_graph'], graph_simi)
            DoConf().makeoptions([self.parametres['type']], [self.parametres], self.pathout['Analyse.ira'])
            if self.parametres['type_graph'] == 1:
                if self.parametres['svg'] :
                    self.graphpan.sizer_3.Add(hl.HyperLinkCtrl(self.graphpan.panel_1, -1, fileout, URL = fileout), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                else :
                    self.graphpan.sizer_3.Add(wx.StaticBitmap(self.graphpan.panel_1, -1, wx.Bitmap(fileout, wx.BITMAP_TYPE_ANY)), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                self.graphpan.sizer_3.Add(wx.StaticText(self.graphpan.panel_1,-1, self.script.txtgraph), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                self.graphpan.sizer_3.Fit(self.graphpan.panel_1)
                self.graphpan.Layout()
                self.graphpan.panel_1.Scroll(0,self.graphpan.panel_1.GetScrollRange(wx.VERTICAL))
        else :
            self.dial.Destroy()

    def make_param(self) :
        if self.parametres['first'] :
            keep_coord = False
        else :
            keep_coord = self.dial.check_coord.GetValue()
        #self.select = self.dial.check_colch.GetValue()
        paramsimi = {'coeff' : self.dial.choice1.GetSelection(),
                          'layout' : self.dial.choice2.GetSelection(),
                          'type_graph' : self.dial.choice3.GetSelection(),
                          'arbremax' : self.dial.check1.GetValue(),
                          'coeff_tv' : self.dial.check_s_size.GetValue(),
                          'coeff_tv_nb' : self.dial.spin_tv.GetValue(),
                          'tvprop' : self.dial.check2.GetValue(),
                          'tvmin' : self.dial.spin_tvmin.GetValue(),
                          'tvmax' : self.dial.spin_tvmax.GetValue(),
                          'coeff_te' : self.dial.check3.GetValue(),
                          'coeff_temin' : self.dial.spin_temin.GetValue(),
                          'coeff_temax' : self.dial.spin_temax.GetValue(),
                          'label_e' : self.dial.check_elab.GetValue(),
                          'label_v' : self.dial.check_vlab.GetValue(),
                          'vcex' : self.dial.check_vcex.GetValue(),
                          'vcexmin' : self.dial.spin_vcexmin.GetValue(),
                          'vcexmax' : self.dial.spin_vcexmax.GetValue(),
                          'cex' : self.dial.spin_cex.GetValue(),
                          'seuil_ok' : self.dial.check_seuil.GetValue(),
                          'seuil' : self.dial.spin_seuil.GetValue(),
                          'cols' : self.dial.cols.GetColour(),
                          'cola' : self.dial.cola.GetColour(),
                          'width' : self.dial.spin_width.GetValue(),
                          'height' : self.dial.spin_height.GetValue(),
                          'first' : False,
                          'keep_coord' : keep_coord,
                          'alpha' : self.dial.slider_sphere.GetValue(),
                          'film' : self.dial.film.GetValue(),
                          'svg' : self.dial.choix_format.GetSelection(),
                          'halo' : self.dial.halo.GetValue(),
                          'com' : self.dial.comcheck.GetValue(),
                          'communities' : self.dial.choix_com.GetSelection(),
                          'edgecurved' : self.dial.check_curved.GetValue(),
                          }
        if 'cexfromchi' in self.parametres :
            paramsimi['cexfromchi'] = self.dial.checkit.GetValue()
        if 'sfromchi' in self.parametres :
            paramsimi['sfromchi'] = self.dial.checki.GetValue()
        if 'vlabcolor' in self.parametres :
            paramsimi['vlabcolor'] = self.parametres['vlabcolor']
        if 'check_bystar' in dir(self.dial) :
            paramsimi['bystar'] = self.dial.check_bystar.GetValue()
            paramsimi['stars'] = self.parametres['stars']
        self.parametres.update(paramsimi)

    def DoR(self, dlg):
        if self.parametres['type_graph'] == 1 :
            graph = False
            wait = False
        else :
            graph = True
            wait = True
        pid = exec_rcode(self.parent.RPath, self.tmpfile, wait = wait, graph = graph)
        if self.parametres['type_graph'] == 1 :
            while pid.poll() == None :
                dlg.Pulse('R ...')
                sleep(0.2)
            check_Rresult(self.parent, pid)

    def export(self, evt) :
        nb = 1
        while os.path.exists(os.path.join(self.pathout.dirout,'graph_'+str(nb)+'.graphml')):
            nb +=1
        fileout = ffr(os.path.join(self.pathout.dirout,'graph_'+str(nb)+'.graphml'))
        txt = """
        library(igraph)
        load("%s")
        source("%s")
        fileout <- "%s"
        graph <- graph.simi$graph
        V(graph)$x <- graph.simi$layout[,1]
        V(graph)$y <- graph.simi$layout[,2]
        if (length(graph.simi$label.cex == 1)) {
            V(graph)$weight <- graph.simi$mat.eff
        } else {
            V(graph)$weight <- graph.simi$label.cex
        }
        V(graph)$color <- vertex.label.color
        V(graph)$frequences <- graph.simi$mat.eff
        V(graph)$fprop <- graph.simi$mat.eff/nrow(dm)
        V(graph)$label <- as.character(graph.simi$v.label)
        E(graph)$weight <- graph.simi$we.width
        write.graph(graph, fileout, format = 'graphml')
        #saveAsGEXF(graph, filepath = fileout)
        """ % (ffr(self.pathout['RData.RData']), ffr(self.parent.RscriptsPath['simi']), fileout)
        filetmp = tempfile.mktemp()
        with open(filetmp, 'w', encoding='utf8') as f :
            f.write(txt)
        exec_rcode(self.ira.RPath, filetmp)
        mss = wx.MessageDialog(self.ira, fileout, _('File exported'), wx.OK)
        mss.CenterOnParent()
        mss.ShowModal()
        mss.Destroy()

    def blender(self, evt):
        blender(self)


class GraphPanelSimi(wx.Panel):

    def __init__(self,parent, dico, list_graph):
        wx.Panel.__init__(self,parent)
        self.afcnb = 1
        self.Dict = dico
        self.dirout = os.path.dirname(self.Dict['ira'])
        self.parent = self.GetParent()
        #self.SetFont(wx.Font(10, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, 0, "courier")) #modifié
        self.labels = []
        self.listimg = []
        self.tabsimi = self.parent.GetParent()
        self.ira = self.tabsimi.GetParent()
        self.panel_1 = wx.ScrolledWindow(self, -1, style=wx.TAB_TRAVERSAL)
        afc_img = wx.Image(os.path.join(self.ira.images_path,'button_simi.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        self.butafc = wx.BitmapButton(self, -1, afc_img)
        export_img = wx.Image(os.path.join(self.ira.images_path,'button_export.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        self.butexport = wx.BitmapButton(self, -1, export_img)
        export_img = wx.Image(os.path.join(self.ira.images_path,'button_exportmat.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        self.butexportmat = wx.BitmapButton(self, -1, export_img)
        #blender_img = wx.Image(os.path.join(self.ira.images_path,'button_blender.png'), wx.BITMAP_TYPE_ANY)
        #blender_img.Rescale(32,32)
        #blender_img = blender_img.ConvertToBitmap()
        #self.butblender = wx.BitmapButton(self, -1, blender_img)
        for i in range(0,len(list_graph)):
            if os.path.exists(os.path.join(self.dirout,list_graph[i][0])) :
                filename, ext = os.path.splitext(list_graph[i][0])
                if ext in ['.svg', '.html'] :
                    self.listimg.append(hl.HyperLinkCtrl(self.panel_1, -1, os.path.join(self.dirout,list_graph[i][0]), URL=os.path.join(self.dirout,list_graph[i][0])))
                else :
                    self.listimg.append(wx.StaticBitmap(self.panel_1, -1, wx.Bitmap(os.path.join(self.dirout,list_graph[i][0]), wx.BITMAP_TYPE_ANY)))
                self.labels.append(wx.StaticText(self.panel_1, -1, list_graph[i][1] + ''))
        self.panel_1.Bind(wx.EVT_MOTION, self.onMouseMove)
        self.__set_properties()
        self.__do_layout()

    def __set_properties(self):
        self.panel_1.EnableScrolling(True,True)
        #self.panel_1.SetSize((1000,1000))
        self.panel_1.SetScrollRate(20, 20)
        self.panel_1.SetFocus()

    def __do_layout(self):
        self.sizer_1 = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer_2 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_3 = wx.BoxSizer(wx.VERTICAL)
        self.sizer_2.Add(self.butafc, 0, 0, 0)
        self.sizer_2.Add(self.butexport, 0, 0, 0)
        self.sizer_2.Add(self.butexportmat, 0, 0, 0)
        #self.sizer_2.Add(self.butblender, 0, 0, 0)
        for i in range(0, len(self.listimg)):
            self.sizer_3.Add(self.listimg[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
            self.sizer_3.Add(self.labels[i], 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        self.panel_1.SetSizer(self.sizer_3)
        self.sizer_1.Add(self.sizer_2, 0, wx.EXPAND, 0)
        self.sizer_1.Add(self.panel_1, 1, wx.EXPAND, 0)
        self.SetSizer(self.sizer_1)

    def onMouseMove(self, event):
        self.panel_1.SetFocus()

