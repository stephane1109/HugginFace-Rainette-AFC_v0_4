# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import sys
import locale
import tempfile
import codecs
import os
from random import randint
from configparser import ConfigParser, RawConfigParser
import webbrowser
import logging

from optparse import OptionParser
parser = OptionParser()
parser.add_option("-f", "--file", dest="filename",
                  help="open FILE", metavar="FILE", default=False)
(options, args) = parser.parse_args()

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.adv
import wx.lib.agw.aui as aui
import wx.html
import wx.grid
import wx.lib.agw.hyperlink as hl

#------------------------------------
# import des fichiers du projet
#------------------------------------
from analyse_merge import AnalyseMerge
from checkinstall import CreateIraDirectory, CheckRPath, FindRPAthWin32, FindRPathNix, CheckRPackages, IsNew, UpgradeConf, CopyConf, RLibsAreInstalled
from checkversion import NewVersion
from chemins import RscriptsPath, ConstructConfigPath, ConstructDicoPath, ConstructGlobalPath, PathOut
from corpus import Builder, SubBuilder, MergeClusters
from dialog import PrefDialog
from functions import BugReport, PlaySound, History, progressbar
from guifunct import *
from openanalyse import OpenAnalyse
from parse_dmi import ImportDMI
from parse_factiva_xml import ImportFactiva
from tabafcm import DoAFCM
from tabchdalc import AnalyseQuest
from tabchddist import ChdCluster
from tabchi2 import ChiSquare
from tabchi2mcnemar import McNemar
from tabfrequence import Frequences, FreqMultiple
from tableau import Tableau
from tabrsimple import InputText
from tabsimi import DoSimi
from tabcatego import Categorisation
from tabsplitvar import SplitMatrixFromVar
from tabverges import Prototypical
from textaslexico import Lexico
from textlabbe import DistLabbe
from textreinert import Reinert
from textsimi import SimiTxt, SimiFromCluster
from textstat import Stat
from textwordcloud import WordCloud, ClusterCloud
from tools import Extract
from tree import LeftTree

import langue
langue.run()


#------------------------------------
# les ID uniques pour tous les éléments qui vont en avoir besoin
#------------------------------------
ID_OpenData = wx.Window.NewControlId()
ID_Import = wx.Window.NewControlId()
ID_OpenText = wx.Window.NewControlId()
ID_OnOpenAnalyse = wx.Window.NewControlId()
ID_Freq = wx.Window.NewControlId()
ID_Chi2 = wx.Window.NewControlId()
ID_Chi2mc = wx.Window.NewControlId()
ID_Student = wx.Window.NewControlId()
ID_CHDSIM = wx.Window.NewControlId()
ID_CHDReinert = wx.Window.NewControlId()
ID_TEXTAFCM = wx.Window.NewControlId()
ID_TEXTSTAT = wx.Window.NewControlId()
ID_ASLEX = wx.Window.NewControlId()
ID_TEXTREINERT = wx.Window.NewControlId()
ID_TEXTPAM = wx.Window.NewControlId()
ID_CHECKCORPUS = wx.Window.NewControlId()
ID_Tabcontent = wx.Window.NewControlId()
ID_AFCM = wx.Window.NewControlId()
ID_SIMI = wx.Window.NewControlId()
ID_CATE = wx.Window.NewControlId()
ID_CloseTab = wx.Window.NewControlId()
ID_SaveTab = wx.Window.NewControlId()
ID_CreateText = wx.Window.NewControlId()
ID_ACCEUIL = wx.Window.NewControlId()
ID_RESULT = wx.Window.NewControlId()
ID_HTMLcontent = wx.Window.NewControlId()
ID_SimiTxt = wx.Window.NewControlId()
ID_proto = wx.Window.NewControlId()
ID_ImportTXM = wx.Window.NewControlId()
ID_FreqMulti = wx.Window.NewControlId()
ID_Splitfromvar = wx.Window.NewControlId()
ID_Subtxtfrommeta = wx.Window.NewControlId()
ID_Subtxtfromthem = wx.Window.NewControlId()
ID_WC = wx.Window.NewControlId()
ID_ImportEuro = wx.Window.NewControlId()
ID_Fact_xml = wx.Window.NewControlId()
ID_Fact_mail = wx.Window.NewControlId()
ID_Fact_copy = wx.Window.NewControlId()
ID_exportmeta = wx.Window.NewControlId()
ID_importdmi = wx.Window.NewControlId()
ID_merge = wx.Window.NewControlId()
ID_merge_clusters = wx.Window.NewControlId()
ID_labbe = wx.Window.NewControlId()

#------------------------------------
# elements de configuration
#------------------------------------

#encodage
# if sys.platform == 'darwin' :
#    sys.setdefaultencoding('UTF-8')
#    wx.SetDefaultPyEncoding('UTF-8')
# else :
#    sys.setdefaultencoding(locale.getpreferredencoding())

#chemin de l'application
AppliPath = os.path.abspath(os.path.dirname(os.path.realpath(sys.argv[0])))

#chemin des images
ImagePath = os.path.join(AppliPath, 'images')

#configuration generale
DictConfigPath = ConstructGlobalPath(AppliPath)
ConfigGlob = ConfigParser()
ConfigGlob.read(DictConfigPath['global'])
DefaultConf = ConfigParser()
DefaultConf.read(DictConfigPath['preferences'])

#repertoire de l'utilisateur
user_home = os.getenv('HOME')
if user_home is None :
    user_home = os.path.expanduser('~')

UserConfigPath = os.path.abspath(os.path.join(user_home, '.iramuteq-%s' % ConfigGlob.get('DEFAULT', 'version_nb')))
ConfigPath = ConstructConfigPath(UserConfigPath)

#Si pas de fichiers de config utilisateur, on cree le repertoire
CreateIraDirectory(UserConfigPath, AppliPath)

#fichiers log pour windows (py2exe)
log = logging.getLogger('iramuteq')
fh = logging.FileHandler(os.path.join(UserConfigPath,'stdout.log'))
formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
fh.setFormatter(formatter)
log.addHandler(fh)
if sys.platform != 'win32' and sys.platform != 'darwin':
    ch = logging.StreamHandler()
    ch.setFormatter(formatter)
    log.addHandler(ch)
log.setLevel(logging.INFO)


class writer(object):

    def write(self, data):
        if data.strip() != '' :
            log.info('ERROR : %s' % data)


class printer(object) :

    def write(self, data) :
        if data.strip() != '' :
            log.info('Print : %s' % data)

    # pour eviter des lignes de log d'erreur
    def flush(self):
        pass

#sys.stderr = writer()
#sys.stdout = printer()

images_analyses = {
        'textroot' : 'textroot.png',
        'alceste' : 'reinert.png',
        'reinert' : 'reinert.png',
        'corpus' : 'textcorpus.png',
        'wordcloud' :'wordcloud.png',
        'stat' :'stats.png',
        'simitxt' : 'simitxt.png',
        'clustersimitxt' :'clustersimitxt.png',
        'clustercloud' : 'clustercloud.png',
        'spec' : 'spec.png',
        'matroot' : 'matroot.png',
        'matrix' : 'matrix.png',
        'freq' : 'frequences.png',
        'freqmulti' : 'frequences.png',
        'chi2' : 'chi2.png',
        'chi2mcnemar' : 'chi2.png',
        'reinertmatrix' : 'reinertmatrix.png',
        'simimatrix' : 'simimatrix.png',
        'simiclustermatrix' : 'simimatrix.png',
        'proto' : 'proto.png',
        'TXM' : 'TXM.png',
        'europress' : 'europress.png',
        'factiva_xml' : 'factiva_xml.png',
        'factiva_copy' : 'factiva_copy.png',
        'factiva_mail': 'factiva_mail.png',
        'iramuteq' : 'iraicone.png',
        'subcorpusmeta' : 'subcorpusmeta.png',
        'subcorpusthema' : 'subcorpusthema.png',
        'preferences' : 'preferences.png',
        'exportmetatable' : 'exportmetatable.png',
        'importdmi' : 'twitter.png',
        'labbe' : 'labbe.png',
        'categorisation' : 'spec.png',
         }


#------------------------------------
# l'ensemble du contexte de Iramuteq : menu, fenetre, etc.
#------------------------------------
class IraFrame(wx.Frame):

    def __init__(self, parent,
                 id= -1, title="",
                 pos=wx.DefaultPosition,
                 size=wx.DefaultSize,
                 style=wx.DEFAULT_FRAME_STYLE |
                       wx.SUNKEN_BORDER |
                       wx.CLIP_CHILDREN):
        log.info('Starting Iramuteq... ' )
        log.info('version : %s' % ConfigGlob.get('DEFAULT', 'version'))
        wx.Frame.__init__(self, parent, id, title, pos, size, style)

        # configuration
        self.AppliPath = AppliPath
        self.images_path = os.path.join(AppliPath,'images')
        self.UserConfigPath = UserConfigPath
        #self.RscriptsPath = ConstructRscriptsPath(AppliPath)
        self.RscriptsPath = PathOut(dirout=os.path.join(AppliPath, 'Rscripts'))
        self.RscriptsPath.basefiles(RscriptsPath)
        #self.DictPath = ConstructDicoPath(AppliPath)
        self.DictPath = ConstructDicoPath(UserConfigPath)
        self.ConfigGlob = ConfigGlob
        self.ConfigPath = ConstructConfigPath(self.UserConfigPath)
        self.pref = RawConfigParser()
        # workaround for import problem
        self.SimiFromCluster = SimiFromCluster
        # tell FrameManager to manage this frame
        self._mgr = aui.AuiManager()
        self._mgr.SetManagedWindow(self)
        self.x = 0
        #Font
        try :
            self.pref.read(self.ConfigPath['preferences'])
            self.fontsize = self.pref.getint('iramuteq','fontsize')
        except :
            self.fontsize = 12
        self.SetFont(wx.Font(self.fontsize, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL))

        #--------------------------------------------------------------------------------
        # creation menu
        #--------------------------------------------------------------------------------
        self.images_analyses = images_analyses
        for img in images_analyses :
            self.images_analyses[img] = wx.Image(os.path.join(self.images_path, self.images_analyses[img]),
                wx.BITMAP_TYPE_PNG).Scale(16,16).ConvertToBitmap()
        self.mb = wx.MenuBar()

        # menu 'Fichier' de la barre de menu (en haut de l'écran)
        file_menu = wx.Menu()
        item = wx.MenuItem(file_menu, ID_OpenData, _("Open a matrix"), _("Open a matrix"))
        #item.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_FILE_OPEN))
        item.SetBitmap(self.images_analyses['matroot'])
        file_menu.Append(item)
        item = wx.MenuItem(file_menu, ID_OpenText, _("Open a text corpus"), _("Open a text corpus"))
        item.SetBitmap(self.images_analyses['textroot'])
        file_menu.Append(item)
        item = wx.MenuItem(file_menu, ID_OnOpenAnalyse, _("Open an analysis"), _("Open an analysis"))
        item.SetBitmap(self.images_analyses['iramuteq'])
        file_menu.Append(item)
        item = wx.MenuItem(file_menu, ID_ImportTXM, _("Import from TXM"), _("Import from TXM"))
        item.SetBitmap(self.images_analyses['TXM'])
        file_menu.Append(item)
        item = wx.MenuItem(file_menu, ID_ImportEuro, _("Import from Europress"), _("Import from Europress"))
        item.SetBitmap(self.images_analyses['europress'])
        file_menu.Append(item)
        item = wx.MenuItem(file_menu, ID_importdmi, _("Import from DMI-TCAT (exp.)"), _("Import from DMI-TCAT (exp.)"))
        item.SetBitmap(self.images_analyses['importdmi'])
        file_menu.Append(item)
        item = wx.MenuItem(file_menu, ID_merge, _('Merge graphs'), _('Merge graphs'))
        #file_menu.Append(item)
        item = wx.MenuItem(file_menu, ID_merge_clusters, _('Corpus from merge clusters'), _('Corpus from merge clusters'))
        file_menu.Append(item)

        # menu Factiva
        menuFactiva = wx.Menu()
        fact_from_xml = wx.MenuItem(menuFactiva, ID_Fact_xml, _("from xml"))
        fact_from_xml.SetBitmap(self.images_analyses['factiva_xml'])
        fact_from_mail = wx.MenuItem(menuFactiva, ID_Fact_mail, _("from mail"))
        fact_from_mail.SetBitmap(self.images_analyses['factiva_mail'])
        fact_from_txt = wx.MenuItem(menuFactiva, ID_Fact_copy, _("from copy/paste"))
        fact_from_txt.SetBitmap(self.images_analyses['factiva_copy'])
        menuFactiva.Append(fact_from_xml)
        menuFactiva.Append(fact_from_mail)
        menuFactiva.Append(fact_from_txt)
        file_menu.Append(-1, _("Import from factiva"), menuFactiva)

        menuTools = wx.Menu()
        splitvar = wx.MenuItem(menuTools, wx.ID_ANY, _("Split from variable"))
        extractmod = wx.MenuItem(menuTools, wx.ID_ANY, _("Extract mods"))
        extractthem = wx.MenuItem(menuTools, wx.ID_ANY, _("Extract thematics"))
        menuTools.Append(splitvar)
        menuTools.Append(extractmod)
        menuTools.Append(extractthem)
        self.ID_splitvar = splitvar.GetId()
        self.ID_extractmod = extractmod.GetId()
        self.ID_extractthem = extractthem.GetId()
        file_menu.Append(-1, _("Tools"), menuTools)

        # ???
        #item = wx.MenuItem(file_menu, ID_SaveTab, _(u"Save tab as..."), _(u"Save tab as..."))
        #item.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_FILE_SAVE_AS))
        #file_menu.AppendItem(item)

        file_menu.Append(wx.ID_EXIT, _("Exit"))
        # sous macOS cet élément est apparemment déplacé automatiquement vers le menu 'pomme' ???

        # menu 'Edition' de la barre de menu (en haut de l'écran)
        # sous macOS, il est déplacé dans le menu 'App'
        # alors que le menu édition (copier/coller etc. reste vide)
        edit_menu = wx.Menu()
        pref = wx.MenuItem(edit_menu, wx.ID_PREFERENCES, _('Preferences'))
        pref.SetBitmap(self.images_analyses['preferences'])
        edit_menu.Append(pref)

        # menu 'Vue' de la barre de menu (en haut de l'écran)
        view_menu = wx.Menu()
        home = wx.MenuItem(view_menu, ID_ACCEUIL, _("Home page"))
        home.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_GO_HOME, size = (16,16)))
        view_menu.Append(home)
        results = wx.MenuItem(view_menu, ID_RESULT, _('Show results'))
        results.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_LIST_VIEW, size = (16,16)))
        view_menu.Append(results)

        # menu 'Analyses de matrice' de la barre de menu (en haut de l'écran)
        matrix_menu = wx.Menu()
        matanalyses = [[ID_Freq, _("Frequencies"), 'freq'],
                       [ID_FreqMulti, _("Multiple  Frequencies"), 'freqmulti'],
                       [ID_Chi2, _("Chi2"), 'chi2'],
                       [ID_Chi2mc, _("Chi2 McNemar"), 'chi2mcnemar'],
                       {'name' : _("Clustering"),
                        'content' : [[ID_CHDReinert, _("Reinert's Method"), 'reinertmatrix']]
                       },
                       [ID_SIMI, _("Similarities Analysis"), 'simimatrix'],
                       [ID_proto, _("Prototypical Analysis"), 'proto'],
                       [ID_Splitfromvar, _("Split from variable"), 'subcorpusmeta'],
                       [ID_CATE, _("ElCaTeGoRiZatoR"), 'categorisation'],
                      ]
        for analyse in matanalyses :
            if not isinstance(analyse, dict) :
                item = wx.MenuItem(matrix_menu, analyse[0], analyse[1])
                item.SetBitmap(self.images_analyses.get(analyse[2], wx.Bitmap(16,16)))
                matrix_menu.Append(item)
            else :
                nmenu = wx.Menu()
                for subana in analyse['content'] :
                    item = wx.MenuItem(nmenu, subana[0], subana[1])
                    item.SetBitmap(self.images_analyses.get(subana[2], wx.Bitmap(16,16)))
                    nmenu.Append(item)
                matrix_menu.Append(-1, analyse['name'], nmenu)
        self.matrix_menu = matrix_menu

        # menu 'Analyse de texte' de la barre de menu (en haut de l'écran)
        text_menu = wx.Menu()
        analyses_text = [[ID_TEXTSTAT, _("Statistics"), 'stat'],
                         [ID_ASLEX, _("Specificities and CA"), 'spec'],
                         [ID_labbe, _("Labbe Distance"),'labbe'],
                         {'name' : _("Clustering"),
                          'content' : [[ID_TEXTREINERT, _("Reinert's Method"), 'alceste']]
                         },
                         [ID_SimiTxt, _("Similarities Analysis"), 'simitxt'],
                         [ID_WC, _("WordCloud"), 'wordcloud'],
                         {'name' : _("Sub corpus"),
                          'content' : [[ID_Subtxtfrommeta, _('Sub corpus from metadata'), 'subcorpusmeta'],
                                       [ID_Subtxtfromthem, _('Sub corpus from thematic'), 'subcorpusthema']]
                         },
                         [ID_exportmeta, _("Export metadata table"), 'exportmetatable'],
                        ]
        for analyse in analyses_text :
            if not isinstance(analyse, dict) :
                item = wx.MenuItem(text_menu, analyse[0], analyse[1])
                item.SetBitmap(self.images_analyses.get(analyse[2], wx.Bitmap(16,16)))
                text_menu.Append(item)
            else :
                nmenu = wx.Menu()
                for subana in analyse['content'] :
                    item = wx.MenuItem(nmenu, subana[0], subana[1])
                    item.SetBitmap(self.images_analyses.get(subana[2], wx.Bitmap(16,16)))
                    nmenu.Append(item)
                text_menu.Append(-1, analyse['name'], nmenu)
        self.text_menu = text_menu

        # menu 'Aide' et 'A propos' de la barre de menu (en haut de l'écran)
        # mais le "à propos est déplacé par macOS sous le menu "Pomme"
        # et il n'a pas d'action apparemment
        help_menu = wx.Menu()
        about = wx.MenuItem(help_menu, wx.ID_ABOUT, _("About..."))
        about.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_INFORMATION, size = (16,16)))
        help_menu.Append(about)
        help = wx.MenuItem(help_menu, wx.ID_HELP, _("Online help..."))
        help.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_HELP, size = (16,16)))
        help_menu.Append(help)

        # après avoir construit chaque menu, on les ajoute à barre de menu (en haut de l'écran)
        self.mb.Append(file_menu, _("File"))
        self.mb.Append(edit_menu, _("Edition"))
        self.mb.Append(view_menu, _("View"))
        self.mb.Append(matrix_menu, _("Matrix analysis"))
        self.mb.Append(text_menu, _("Text analysis"))
        self.mb.Append(help_menu, _("Help"))
        self.SetMenuBar(self.mb)

        #--------------------------------------------------------------------
        # barre de statut : sur macOS, c'est la barre en bas de la fenêtre Iramuteq
        #--------------------------------------------------------------------
        self.statusbar = self.CreateStatusBar(2, wx.STB_SIZEGRIP)
        self.statusbar.SetStatusWidths([-2, -3])
        self.statusbar.SetStatusText(_("Ready"), 0)
        self.statusbar.SetStatusText(_("Welcome"), 1)
        # min size for the frame itself isn't completely done.
        # see the end up FrameManager::Update() for the test
        # code. For now, just hard code a frame minimum size
        self.SetMinSize(wx.Size(800, 600))

        #--------------------------------------------------------------------
        # barre d'outils : le menu de petits icones en haut de la fenetre
        # il y en a 4 : tb1, tb_text, tb_mat, tb_help
        #--------------------------------------------------------------------
        # tb1
        tb1 = wx.ToolBar(self, -1, wx.DefaultPosition, wx.DefaultSize, wx.TB_FLAT | wx.TB_NODIVIDER)
        tb1.SetToolBitmapSize(wx.Size(16, 16))
        tb1.AddTool(ID_OpenData, "OpenData", self.images_analyses['matroot'], shortHelp=_("Open a matrix"))
        tb1.AddSeparator()
        tb1.AddTool(ID_OpenText, "OpenText", self.images_analyses['textroot'], shortHelp=_("Open a text corpus"))
        tb1.AddSeparator()
        tb1.AddTool(ID_OnOpenAnalyse, "OpenAnalyse", self.images_analyses['iramuteq'], shortHelp= _("Open an analysis"))
        tb1.AddSeparator()
        tb1.AddTool(ID_ImportTXM, "ImportTXM", self.images_analyses['TXM'], shortHelp= _("Import from TXM"))
        tb1.AddSeparator()
        tb1.AddTool(ID_ImportEuro, "ImportEuro", self.images_analyses['europress'], shortHelp= _("Import from Europress"))
        tb1.AddSeparator()
        tb1.AddTool(ID_importdmi, "ImportDMI", self.images_analyses['importdmi'], shortHelp= _("Import from DMI-TCAT (exp.)"))
        tb1.AddSeparator()
        tb1.AddTool(ID_Fact_xml, "ImportFactxml", self.images_analyses['factiva_xml'], shortHelp= _("Factiva from xml"))
        tb1.AddTool(ID_Fact_mail, "ImportFactmail", self.images_analyses['factiva_mail'], shortHelp= _("Factiva from mail"))
        tb1.AddTool(ID_Fact_copy, "ImportFactcopy", self.images_analyses['factiva_copy'], shortHelp= _("Factiva from copy/paste"))
        tb1.AddSeparator()
        tb1.AddTool(wx.ID_PREFERENCES, "Preferences", self.images_analyses['preferences'], shortHelp= _("Preferences"))
        tb1.AddSeparator()
        tb1.AddTool(ID_ACCEUIL, "Home", wx.ArtProvider.GetBitmap(wx.ART_GO_HOME, size = (16,16)), shortHelp= _("Home page"))
        tb1.AddTool(ID_RESULT, "Results", wx.ArtProvider.GetBitmap(wx.ART_LIST_VIEW, size = (16,16)), shortHelp= _('Show results'))
        tb1.Realize()
        # tb_text
        tb_text = wx.ToolBar(self, -1, wx.DefaultPosition, wx.DefaultSize, wx.TB_FLAT | wx.TB_NODIVIDER)
        for analyse in analyses_text :
            if not isinstance(analyse, dict) :
                tb_text.AddTool(analyse[0], analyse[1], self.images_analyses.get(analyse[2], wx.Bitmap(16,16)), shortHelp = analyse[1])
            else :
                for subana in analyse['content'] :
                    tb_text.AddTool(subana[0], subana[1], self.images_analyses.get(subana[2], wx.Bitmap(16,16)), shortHelp = subana[1])
        tb_text.Realize()
        # tb_mat
        tb_mat = wx.ToolBar(self, -1, wx.DefaultPosition, wx.DefaultSize, wx.TB_FLAT | wx.TB_NODIVIDER)
        for analyse in matanalyses :
            if not isinstance(analyse, dict) :
                tb_mat.AddTool(analyse[0], analyse[1], self.images_analyses.get(analyse[2], wx.Bitmap(16,16)), shortHelp = analyse[1])
            else :
                for subana in analyse['content'] :
                    tb_mat.AddTool(subana[0], subana[1], self.images_analyses.get(subana[2], wx.Bitmap(16,16)), shortHelp = subana[1])
        tb_mat.Realize()
        #tb_help
        tb_help = wx.ToolBar(self, -1, wx.DefaultPosition, wx.DefaultSize, wx.TB_FLAT | wx.TB_NODIVIDER)
        tb_help.AddTool(wx.ID_ABOUT, "About", wx.ArtProvider.GetBitmap(wx.ART_INFORMATION, size=(16,16)), shortHelp=_("About..."))
        tb_help.AddTool(wx.ID_HELP, "Help", wx.ArtProvider.GetBitmap(wx.ART_HELP, size=(16,16)), shortHelp=_("Online help..."))
        tb_help.Realize()

        # ???
        self.text_ctrl_txt = wx.TextCtrl(self, -1, "", wx.Point(0, 0), wx.Size(200, 200), wx.NO_BORDER | wx.TE_MULTILINE | wx.TE_RICH2 | wx.TE_READONLY)
        self._mgr.AddPane(self.text_ctrl_txt, aui.AuiPaneInfo().Name("Text").CenterPane())
        self._mgr.AddPane(IntroPanel(self), aui.AuiPaneInfo().Name("Intro_Text").CenterPane())

        #------------------------------------------------------------------------------------------------
        # fichier d'historique de Iramuteq
        #------------------------------------------------------------------------------------------------
        if not os.path.exists(os.path.join(UserConfigPath, 'history.db')) :
            with open(os.path.join(UserConfigPath, 'history.db'), 'w', encoding='utf8') as f :
                f.write('{}')
        self.history = History(os.path.join(UserConfigPath, 'history.db'))
        # l'extension ".db" est ajoutée automatiquement par le module

        #------------------------------------------------------------------------------------------------
        # colonne gauche de la fenetre de Iramuteq, classe "Lefttree"
        #------------------------------------------------------------------------------------------------
        #self.history.dostat()
        self.tree = LeftTree(self)
        self._mgr.AddPane(self.tree,
            aui.AuiPaneInfo().
            Name("lefttree").
            Caption(_("Historic")).
            Left().
            MinSize(wx.Size(200,400)).
            BestSize(wx.Size(300,-1)).
            Layer(1).
            Position(1).
            CloseButton(False).
            MaximizeButton(True).
            MinimizeButton(True))

        self.nb = aui.AuiNotebook(self,
            -1,
            wx.DefaultPosition,
            wx.DefaultSize,
            aui.AUI_NB_DEFAULT_STYLE)
            # | aui.AUI_NB_TAB_EXTERNAL_MOVE | aui.AUI_NB_TAB_MOVE | aui.AUI_NB_TAB_FLOAT | wx.NO_BORDER)
        notebook_flags = aui.AUI_NB_DEFAULT_STYLE
        # | aui.AUI_NB_TAB_EXTERNAL_MOVE | aui.AUI_NB_TAB_MOVE | aui.AUI_NB_TAB_FLOAT| wx.NO_BORDER
        self.nb.SetAGWWindowStyleFlag(notebook_flags)
        #self.nb.SetArtProvider(aui.VC8TabArt())
        nbart = self.nb.GetArtProvider()
        nbart.SetBaseColour('blue')
        self.nb.SetFont(wx.Font(self.fontsize, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL))
        #self.nb.SetArtProvider(aui.VC8TabArt())
        #self.nb.parent = self
        #self._notebook_style = aui.AUI_NB_DEFAULT_STYLE | aui.AUI_NB_TAB_EXTERNAL_MOVE | wx.NO_BORDER

        #------------------------------------------------------------------------------------------------
        # colonne droite de la fenetre de Iramuteq "Tab_content"
        #------------------------------------------------------------------------------------------------
        self._mgr.AddPane(self.nb,
            aui.AuiPaneInfo().
            Name("Tab_content").
            CenterPane())

        #self._mgr.AddPane(self.Sheet, wx.aui.AuiPaneInfo().Name("Data").CenterPane())
        #self._mgr.AddPane(self.Sheet, aui.AuiPaneInfo().Name("Data").CenterPane())
        self.nb.Bind(aui.EVT_AUINOTEBOOK_PAGE_CLOSED, self.OnCloseTab)
        self.nb.Bind(aui.EVT_AUINOTEBOOK_PAGE_CHANGED, self.OnPageChanged)

        #------------------------------------------------------------------------------------------------
        # ajout des toolbars à la fenetre de Iramuteq
        # Iramuteq n'utilise pas directement les 'toolbar' au sens de wx.python
        # mais en fait des ToolbarPane ???
        #------------------------------------------------------------------------------------------------
        self._mgr.AddPane(tb1, aui.AuiPaneInfo().
            Name("tb1").
            Caption("Fichiers").
            ToolbarPane().
            Top().
            LeftDockable(True).
            RightDockable(False))
        self._mgr.AddPane(tb_text, aui.AuiPaneInfo().
            Name("tb_text").
            Caption("analyse_text").
            ToolbarPane().
            Top().
            LeftDockable(True).
            RightDockable(False))
        self._mgr.AddPane(tb_mat, aui.AuiPaneInfo().
            Name("tb_mat").
            Caption("analyse_matrix").
            ToolbarPane().
            Top().
            LeftDockable(True).
            RightDockable(False))
        self._mgr.AddPane(tb_help, aui.AuiPaneInfo().
            Name("tb_help").
            Caption("help").
            ToolbarPane().
            Top().
            LeftDockable(True).
            RightDockable(False))
# ces deux toolbars sont cachées car elles dépendent du contexte des éléments sélectionnés dans lefttree
        self._mgr.GetPane('tb_text').Hide()
        self._mgr.GetPane('tb_mat').Hide()

        self.ShowAPane("Intro_Text")
        self._mgr.GetPane("lefttree").Show()
        self._mgr.GetPane("classif_tb").Hide() # utilisé nulle part ailleurs que sur cette ligne ???
        # "commit" all changes made to FrameManager
        #self._mgr.Update()

        # Attache les événements aux éléments d'interface
        self.Bind(wx.EVT_MENU, self.OnAcceuil, id=ID_ACCEUIL)
        self.Bind(wx.EVT_MENU, self.ShowTab, id=ID_RESULT)
        self.Bind(wx.EVT_MENU, self.OnOpenData, id=ID_OpenData)
        self.Bind(wx.EVT_MENU, self.OnOpenText, id=ID_OpenText)
        self.Bind(wx.EVT_MENU, self.OnOpenAnalyse, id=ID_OnOpenAnalyse)
        self.Bind(wx.EVT_MENU, self.import_factiva_xml, fact_from_xml)
        self.Bind(wx.EVT_MENU, self.import_factiva_mail, fact_from_mail)
        self.Bind(wx.EVT_MENU, self.import_factiva_txt, fact_from_txt)
        self.Bind(wx.EVT_MENU, self.ExtractTools, splitvar)
        self.Bind(wx.EVT_MENU, self.ExtractTools, extractmod)
        self.Bind(wx.EVT_MENU, self.ExtractTools, extractthem)
        self.Bind(wx.EVT_MENU, self.OnFreq, id=ID_Freq)
        self.Bind(wx.EVT_MENU, self.OnFreqMulti, id=ID_FreqMulti)
        self.Bind(wx.EVT_MENU, self.OnChi2, id=ID_Chi2)
        self.Bind(wx.EVT_MENU, self.OnChi2McNemar, id=ID_Chi2mc)
        self.Bind(wx.EVT_MENU, self.OnStudent, id=ID_Student)
        self.Bind(wx.EVT_MENU, self.OnCHDSIM, id=ID_CHDSIM)
        self.Bind(wx.EVT_MENU, self.OnCHDReinert, id=ID_CHDReinert)
        self.Bind(wx.EVT_MENU, self.OnAFCM, id=ID_AFCM)
        self.Bind(wx.EVT_MENU, self.OnProto, id=ID_proto)
        self.Bind(wx.EVT_MENU, self.OnSplitVar, id = ID_Splitfromvar)
        self.Bind(wx.EVT_MENU, self.OnCategorisation, id = ID_CATE)
        #self.Bind(wx.EVT_MENU, self.OnRCode, id=ID_RCODE) #???
        #self.Bind(wx.EVT_MENU, self.OnSplitVar, id=ID_SPLITVAR) #???
        #self.Bind(wx.EVT_MENU, self.OnCheckcorpus, id = ID_CHECKCORPUS) #???
        self.Bind(wx.EVT_MENU, self.OnTextStat, id=ID_TEXTSTAT)
        self.Bind(wx.EVT_MENU, self.OnTextSpec, id=ID_ASLEX)
        self.Bind(wx.EVT_MENU, self.OnTextLabbe, id=ID_labbe)
        self.Bind(wx.EVT_MENU, self.OnTextAfcm, id=ID_TEXTAFCM)
        self.Bind(wx.EVT_MENU, self.OnTextReinert, id=ID_TEXTREINERT)
        self.Bind(wx.EVT_MENU, self.OnPamSimple, id=ID_TEXTPAM)
        self.Bind(wx.EVT_MENU, self.OnSimiTxt, id=ID_SimiTxt)
        self.Bind(wx.EVT_MENU, self.OnWordCloud, id=ID_WC)
        self.Bind(wx.EVT_MENU, self.OnSubText, id = ID_Subtxtfrommeta)
        self.Bind(wx.EVT_MENU, self.OnSubText, id = ID_Subtxtfromthem)
        self.Bind(wx.EVT_MENU, self.OnSimiTab, id=ID_SIMI)
        self.Bind(wx.EVT_MENU, self.OnExit, id=wx.ID_EXIT)
        #self.Bind(wx.EVT_MENU, self.OnSaveTabAs, id=ID_SaveTab) #???
        self.Bind(wx.EVT_MENU, self.OnAbout, id=wx.ID_ABOUT)
        self.Bind(wx.EVT_MENU, self.OnHelp, id=wx.ID_HELP)
        self.Bind(wx.EVT_MENU, self.OnPref, id=wx.ID_PREFERENCES)
        self.Bind(wx.EVT_MENU, self.OnImportTXM, id=ID_ImportTXM)
        self.Bind(wx.EVT_MENU, self.OnImportEuropress, id=ID_ImportEuro)
        self.Bind(wx.EVT_MENU, self.OnImportDMI, id=ID_importdmi)
        self.Bind(wx.EVT_MENU, self.OnExportMeta, id=ID_exportmeta)
        self.Bind(wx.EVT_MENU, self.OnMergeGraph, id = ID_merge)
        self.Bind(wx.EVT_MENU, self.OnMergeClusters, id = ID_merge_clusters)
        self.Bind(wx.EVT_CLOSE, self.OnClose)

        flags = self._mgr.GetAGWFlags()
        #flags &= ~wx.aui.AUI_MGR_TRANSPARENT_HINT
        #flags &= ~wx.aui.AUI_MGR_VENETIAN_BLINDS_HINT
        #flags &= ~wx.aui.AUI_MGR_RECTANGLE_HINT
        flags &= ~(aui.AUI_MGR_RECTANGLE_HINT | aui.AUI_MGR_ALLOW_FLOATING)
        self._mgr.SetAGWFlags(self._mgr.GetAGWFlags() ^ (aui.AUI_MGR_RECTANGLE_HINT | aui.AUI_MGR_ALLOW_FLOATING))
        self._mgr.GetArtProvider().SetMetric(aui.AUI_DOCKART_GRADIENT_TYPE, aui.AUI_GRADIENT_HORIZONTAL)
        self.GetDockArt().SetColor(aui.AUI_DOCKART_ACTIVE_CAPTION_GRADIENT_COLOUR, "#00FFF9")
        #self.DoUpdate()
        self._icon = wx.Icon(os.path.join(ImagePath, "iraicone.ico"), wx.BITMAP_TYPE_ICO)
        self.SetIcon(self._icon)

        self.ctrl = ""
        self.input_path = [False]
        self.TEMPDIR = tempfile.mkdtemp('iramuteq')
        self.FileTabList = []
        self.listbar=[]
        self.DictTab = {}
        self.FreqNum = 0
        self.colsep = ''
        self.txtsep = ''
        self.g_header = False
        self.g_id = False
        self.table = ''
        self.fileforR = ''
        self.filename = ''
        self.nastrings = ''
        self.encode = ''
        self.SysEncoding = sys.getdefaultencoding()
        self.syscoding = sys.getdefaultencoding()
        if self.SysEncoding == 'mac-roman' : self.SysEncoding = 'MacRoman'
        self.type = ''
        #------------------------------------------------------------------------------------------------
        # 'view', 'matrix' et 'text' sont des valeurs attendues par la fonction ShowMenu
        self.ShowMenu('view', True)
        self.ShowMenu('matrix', False)
        self.ShowMenu('text', False)
        #------------------------------------------------------------------------------------------------
        self._mgr.Update()
        self.DataPop = False
        self.DataTxt = False
        self.Text = ''
        self.lexique = None
        self.corpus = None


    def finish_init(self) :
        try :
            self.pref.read(self.ConfigPath['preferences'])
            if IsNew(self) :
                UpgradeConf(self)
                self.pref.read(self.ConfigPath['preferences'])
                New = True
            else :
                CopyConf(self)
                New = False
        except :
            UpgradeConf(self)
            self.pref.read(self.ConfigPath['preferences'])
            New = True
        self.sound = self.pref.getboolean('iramuteq', 'sound')
        self.check_update = self.pref.getboolean('iramuteq', 'checkupdate')
        self.version = ConfigGlob.get('DEFAULT', 'version')
        # configuration des chemins de R
        self.PathPath = ConfigParser()
        self.PathPath.read(ConfigPath['path'])
        BestRPath = False
        if not CheckRPath(self.PathPath) :
            if sys.platform == 'win32':
                if os.path.exists(self.AppliPath + '\\R\\R\\x64\\R.exe') :
                    BestRPath = self.AppliPath + '\\R\\R\\bin\\x64\\R.exe'
                elif os.path.exists(self.AppliPath + '\\R\\R\\i386\\R.exe') :
                    BestRPath = self.AppliPath + '\\R\\R\\bin\\i386\\R.exe'
                else :
                    BestRPath = FindRPAthWin32()
            elif os.path.exists(self.AppliPath + '/R/R') :
                BestRPath = self.AppliPath + '/R/R'
            else:
                BestRPath = FindRPathNix()
            if BestRPath:
                self.PathPath.set('PATHS', 'rpath', BestRPath)
                with open(ConfigPath['path'], 'w', encoding='utf8') as f :
                    self.PathPath.write(f)
        else:
            BestRPath = True
        if BestRPath :
            self.RPath = self.PathPath.get('PATHS', 'rpath')
            if New :
                CheckRPackages(self)
            if not RLibsAreInstalled(self) :
                CheckRPackages(self)
        else :
            msg = '\n'.join([_("Can't find R executable"), _("If R is not installed, get it from http://www.r-project.org."),
                             _("If R is installed, report its path in Preferences."),
                             _("IRaMuTeQ does not work without R.")])
            dlg = wx.MessageDialog(self, msg, _("Problem"), wx.OK | wx.ICON_WARNING)
            dlg.CenterOnParent()
            if dlg.ShowModal() in [wx.ID_NO, wx.ID_CANCEL]:
                pass
            dlg.Destroy()

    def OnVerif(self, evt) :
        pack = CheckRPackages(self)
        if pack :
            dlg = wx.MessageDialog(self, _("Installation OK"), _("Installation"), wx.OK | wx.ICON_INFORMATION | wx.STAY_ON_TOP)
            dlg.CenterOnParent()
            if dlg.ShowModal() in [wx.ID_NO, wx.ID_CANCEL]:
                evt.Veto()

    # appelé par des fonctions de ce fichier et tree.py : OnSelChanged
    # vu comme elle est écrite, impossible de gérer
    # l'affichage/masquage des toolbars en fonction du contexte
    def ShowMenu(self, menu, Show=True):
        if menu == 'text' :
            menu_pos = 4
            if Show :
                if self._mgr.GetPane('tb_text').IsShown()  :
                    return
                self._mgr.GetPane('tb_text').Show()
                self._mgr.GetPane('tb_mat').Hide()
                self.mb.EnableTop(menu_pos, Show)
                self.mb.EnableTop(3, False)
            else :
                self._mgr.GetPane('tb_text').Hide()
                self.mb.EnableTop(menu_pos, Show)
        elif menu == 'matrix' :
            menu_pos = 3
            if Show :
                if self._mgr.GetPane('tb_mat').IsShown():
                    return
                self._mgr.GetPane('tb_mat').Show()
                self._mgr.GetPane('tb_text').Hide()
                self.mb.EnableTop(menu_pos, Show)
                self.mb.EnableTop(4, False)
            else :
                self._mgr.GetPane('tb_mat').Hide()
                self.mb.EnableTop(menu_pos, Show)
        elif menu == 'view' :
            menu_pos = 2
        else :
            menu_pos = None
        if not menu_pos is None :
            #self.mb.EnableTop(menu_pos, Show)
            self.mb.Refresh()
        self._mgr.Update()
        self.Refresh()

    #--------------------------------------------------------------------
    # fin de __init__ du wx.Frame
    #--------------------------------------------------------------------

    # evenement attaché au bouton de fermeture des fenetres ou onglets ?
    def OnClose(self, event):
        print('onclose Iramuteq')
        with open(self.ConfigPath['path'], 'w', encoding='utf8') as f :
            self.PathPath.write(f)
        self._mgr.UnInit()
        del self._mgr
        self.Destroy()

    # evenement attaché au menu 'ouvrir matrice'
    def OnOpenData(self, event):
        print('on open data')
        inputname, self.input_path = OnOpen(self, "Data")
        if inputname:
            # filename = self.input_path[0]
            self.tableau = Tableau(self,os.path.abspath(self.input_path[0]))
            val = get_table_param(self, self.input_path[0])
            if val == wx.ID_OK :
                busy = wx.BusyInfo(_("Please wait..."), self)
                wx.SafeYield()
                try :
                    self.tableau.make_content()
                    OpenAnalyse(self, self.tableau.parametres)
                    self.tree.OnItemAppend(self.tableau.parametres)
                    del busy
                except :
                    del busy
                    BugReport(self)
                # self.tableau.show_tab()

    # evenement attaché au menu 'ouvrir analyse'
    def OnOpenAnalyse(self, event):
        print('on open analyse')
        self.AnalysePath = OnOpen(self, "Analyse")
        if self.AnalysePath :
            OpenAnalyse(self, self.AnalysePath[1][0], True)
            self.ShowMenu('view')

    # evenement attaché au menu 'ouvrir un texte/corpus'
    def OnOpenText(self, event):
        print('on open text')
        inputname, self.input_path = OnOpen(self, "Texte")
        self.filename = self.input_path[0]
        if inputname:
            self.OpenText()

    # evenement attaché au menu 'ouvrir analyse'
    def OnSubText(self, evt, corpus = None, parametres = None):
        print('on sub text')
        if corpus is None :
            corpus = self.tree.getcorpus()
        if evt.GetId() == ID_Subtxtfrommeta :
            parametres = {'frommeta' : True}
        elif evt.GetId() == ID_Subtxtfromthem :
            parametres = {'fromtheme' : True}
        builder = SubBuilder(self, corpus, parametres)
        if builder.res == wx.ID_OK :
            busy = wx.BusyInfo(_("Please wait..."), self)
            wx.SafeYield()
            corpus = builder.doanalyse()
            self.history.add(corpus.parametres)
            OpenAnalyse(self, corpus.parametres)
            self.tree.OnItemAppend(corpus.parametres)
            del busy

    # action d'ouverture d'un texte
    def OpenText(self):
        print('open text')
        builder =  Builder(self, 5)
        if builder.res == wx.ID_OK :
            try :
                corpus = builder.doanalyse()
                self.history.add(corpus.parametres)
                self.tree.OnItemAppend(corpus.parametres)
                OpenAnalyse(self, corpus.parametres)
            except :
                builder.dlg.Destroy()
                BugReport(self)
            else :
                count = 1
                keepGoing = builder.dlg.Update(count, "Lecture du fichier")
                self.ShowMenu('view')
                self.ShowMenu('text')
                self.ShowMenu('matrix', False)
                self.type = "Texte"
                self.DataTxt = False
                self.Text = ''
                count += 1
                keepGoing = builder.dlg.Update(count, "Chargement du dictionnaire")
                builder.dlg.Destroy()

    # evenement attaché au menu 'quitter'
    def OnExit(self, event):
        self.Close()

    # evenement attaché au menu 'à propos'
    def OnAbout(self, event):
        print('on about')
        info = wx.adv.AboutDialogInfo()
        info.Name = ConfigGlob.get('DEFAULT', 'name')
        info.SetIcon(self._icon)
        info.Version = ConfigGlob.get('DEFAULT', 'version')
        info.Copyright = ConfigGlob.get('DEFAULT', 'copyright')
        info.Translators = ConfigGlob.get('DEFAULT', 'translators').split(';')
        info.Description = """
Interface de R pour les Analyses Multidimensionnelles
de Textes et de Questionnaires

Un logiciel libre
construit avec des logiciels libres.

Laboratoire LERASS

"""
        info.WebSite = ("http://www.iramuteq.org", "Site web IRaMuTeQ")
        dev = ConfigGlob.get('DEFAULT', 'dev').split(';')
        info.Developers = dev
        info.License = """Iramuteq est un logiciel libre ; vous pouvez le diffuser et/ou le modifier
suivant les termes de la Licence Publique Générale GNU telle que publiée
par la Free Software Foundation ; soit la version 2 de cette licence,
soit (à votre convenance) une version ultérieure.

Iramuteq est diffusé dans l'espoir qu'il sera utile,
mais SANS AUCUNE GARANTIE ; sans même une garantie implicite
de COMMERCIALISATION ou d'ADÉQUATION À UN USAGE PARTICULIER.
Voyez la Licence Publique Générale GNU pour plus de détails.

Vous devriez avoir reçu une copie de la Licence Publique Générale GNU
avec Iramuteq ; sinon, veuillez écrire à la Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, États-Unis."""
        wx.adv.AboutBox(info)

    # appelé seulement pour l'initialisation de la fenetre principale de Iramuteq
    def GetDockArt(self):
        return self._mgr.GetArtProvider()

    # appelé seulement pour l'initialisation de la fenetre principale de Iramuteq
    def DoUpdate(self):
        self._mgr.Update()

    # action ou évènement ?
    def OnPageChanged(self, event) :
        new = event.GetSelection()
        nobject = event.GetEventObject()
        parent = nobject.GetParent()
        if isinstance(parent, IraFrame) :
            npage = self.nb.GetPage(new)
            if 'parametres' in dir(npage) :
                self.tree.GiveFocus(uuid=npage.parametres['uuid'])

    # action ou évènement ?
    def OnCloseTab(self, evt):
        #log.info('Closing tab %s' % str(evt.GetEventObject()))
        ctrl = evt.GetEventObject()
        print('page lcose', ctrl)
        if isinstance(ctrl.GetParent(), aui.AuiNotebook) or isinstance(ctrl.GetParent(), wx.Panel):
            notebook = True
        else :
            notebook = False
        page = self.nb.GetPage(ctrl.GetSelection())
        if 'parametres' in dir(page) and isinstance(ctrl.GetParent(), IraFrame) :
            self.history.rmtab(page.parametres)
            self.tree.CloseItem(uuid = page.parametres['uuid'])
        TabTitle = self.nb.GetPageText(self.nb.GetSelection())
        if self.nb.GetPageCount() == 1 and not notebook :
            self.LastTabClose()

    # action ou évènement ?
    def LastTabClose(self) :
        if self.nb.GetPageCount() == 1 :
            if self.DataTxt :
                self.ShowAPane("Text")
            elif self.DataPop :
                self.ShowAPane("Data")
            else :
                self.ShowAPane("Intro_Text")

    # action ou évènement ?
    def GetStartPosition(self):
        self.x = self.x + 20
        x = self.x
        pt = self.ClientToScreen(wx.Point(0, 0))
        return wx.Point(pt.x + x, pt.y + x)

    # action ou évènement ?
    def ShowAPane(self, panel):
        #print('ShowAPane')
        for pane in self._mgr.GetAllPanes() :
            if not pane.IsToolbar() and pane.name != 'lefttree':
                pane.Hide()
        self._mgr.GetPane(panel).Show()
        self._mgr.Update()
        wx.CallAfter(self.nb.SendSizeEvent)
        self.Refresh()

    # action ou évènement ?
    def OnAcceuil(self, event):
        self.ShowAPane("Intro_Text")
        event.Skip()

    # action ou évènement ?
    def CreateHTMLCtrl(self):
        ctrl = wx.html.HtmlWindow(self, -1, wx.DefaultPosition, wx.Size(400, 300))
        if "gtk2" in wx.PlatformInfo:
            ctrl.SetStandardFonts()
        ctrl.SetPage("text")
        return ctrl

    # action ou évènement ?
    def ShowTab(self, evt):
        self.ShowAPane("Tab_content")

    ################################################################
    #debut des analyses
    ################################################################
    def analyse_matrix(self, analyse, analyse_type = '', matrix = None, parametres = None, dlgnb = 1):
        if matrix is None :
            matrix = self.tree.getmatrix()
        if parametres is not None :
            parametres['type'] = analyse_type
        else :
            parametres = {'type' : analyse_type}
        try :
            #print 'plus de bug@@@@@@@@@@@@@@@@@@@@@@'
            analyse(self, matrix, parametres = parametres, dlg = dlgnb)
        except:
            BugReport(self)

    def OnFreq(self, event, matrix = None):
        self.analyse_matrix(Frequences, analyse_type = 'freq', matrix = matrix, dlgnb = 3)

    def OnFreqMulti(self, event, matrix = None):
        self.analyse_matrix(FreqMultiple, analyse_type = 'freqmulti', matrix = matrix, dlgnb = 3)

    def OnChi2(self, event, matrix = None):
        self.analyse_matrix(ChiSquare, matrix = matrix, analyse_type = 'chi2', dlgnb = 3)

    def OnChi2McNemar(self, event, matrix = None):
        self.analyse_matrix(McNemar, matrix = matrix, analyse_type = 'chi2mcnemar', dlgnb = 3)

    def OnSimiTab(self, event, matrix = None):
        self.analyse_matrix(DoSimi, matrix = matrix, analyse_type = 'simimatrix', dlgnb = 5)

    def OnCategorisation(self, event, matrix = None) :
        self.analyse_matrix(Categorisation, matrix = matrix, analyse_type = 'categorisation', dlgnb = 1)


    def OnCHDReinert(self, event, matrix = None):
        #if matrix is None :
        #    matrix = self.tree.getmatrix()
        #AnalyseQuest(self, matrix, parametres = {'type' : 'reinertmatrix'}, dlg = 3)
        self.analyse_matrix(AnalyseQuest, matrix = matrix, analyse_type = 'reinertmatrix', dlgnb = 5)

    def OnStudent(self, event):
        try:
            MakeStudent(self)
        except:
            BugReport(self)

    def OnRCode(self, event):
        try:
            InputText(self)
        except:
            BugReport(self)

    def OnCHDSIM(self, event):
        try:
            chdsim = ChdCluster(self)
            if chdsim.val == wx.ID_OK:
                PlaySound(self)
        except:
            BugReport(self)

#     def OnCHDReinert(self, event):
#         try:
#          #   print('PLUS DE BUG SUR ALCESTE QUESTIONNAIRE')
#             self.quest = AnalyseQuest(self)
#             if self.quest.val == wx.ID_OK:
#                 PlaySound(self)
#         except:
#             BugReport(self)

    def OnMergeGraph(self, evt):
        #FIXME
        AnalyseMerge(self, {'type': 'merge', 'fileout' : '/tmp/test.txt'}, dlg = 5)

    def OnMergeClusters(self, evt) :
        print('on merge clusters')
        builder = MergeClusters(self, {})
        if builder.res == wx.ID_OK :
            busy = wx.BusyInfo(_("Please wait..."), self)
            wx.SafeYield()
            corpus = builder.doanalyse()
            self.history.add(corpus.parametres)
            OpenAnalyse(self, corpus.parametres)
            self.tree.OnItemAppend(corpus.parametres)
            del busy

    def OnProto(self, evt, matrix = None) :
        self.analyse_matrix(Prototypical, matrix = matrix, analyse_type = 'proto', dlgnb = 3) 
        #Prototypical(self, {'type' : 'proto'})

    def OnSplitVar(self, evt, matrix = None):
        if matrix is None :
            matrix = self.tree.getmatrix()
        self.analyse_matrix(SplitMatrixFromVar, matrix = matrix, analyse_type = 'splitvar', parametres = {'pathout': matrix.pathout.dirout}, dlgnb = 3)
        #matrix = self.tree.getmatrix()

    def OnSimiTxt(self, evt, corpus = None) :
        try :
            #self.Text = SimiTxt(self)
            if corpus is None :
                corpus = self.tree.getcorpus()
            self.Text = SimiTxt(self, corpus, parametres = {'type': 'simitxt'}, dlg = 3)
            if self.Text.val == wx.ID_OK :
                PlaySound(self)
        except :
            BugReport(self)

    def OnWordCloud(self, evt, corpus = None) :
        try :
            if corpus is None :
                corpus = self.tree.getcorpus()
            self.Text = WordCloud(self, corpus, parametres = {'type' : 'wordcloud'}, dlg = 3)
            if self.Text.val == wx.ID_OK :
                PlaySound(self)
        except :
            BugReport(self)

    def OnClusterCloud(self, corpus, parametres = None) :
        self.Text = ClusterCloud(self, corpus, parametres = parametres, dlg = 3)

    def OnAFCM(self, event):
        try:
            DoAFCM(self)
        except:
            BugReport(self)

    def OnTextStat(self, event, corpus = None):
        try:
            if corpus is None :
                corpus = self.tree.getcorpus()
            self.Text = Stat(self, corpus, parametres = {'type': 'stat'}, dlg = 7)
            if self.Text.val == wx.ID_OK :
                PlaySound(self)
        except:
            BugReport(self)

    def OnTextSpec(self, event, corpus = None):
        try:
            #self.Text = AsLexico(self)
            if corpus is None :
                corpus = self.tree.getcorpus()
            self.Text = Lexico(self, corpus, parametres = {'type' : 'spec'}, dlg = 3)
            if self.Text.val == wx.ID_OK :
                PlaySound(self)
        except:
            BugReport(self)

    def OnTextLabbe(self, event, corpus = None):
        try:
            if corpus is None :
                corpus = self.tree.getcorpus()
            self.Text = DistLabbe(self, corpus, parametres = {'type' : 'labbe'}, dlg = 3)
            if self.Text.val == wx.ID_OK :
                PlaySound(self)
        except:
            BugReport(self)

    def OnTextAfcm(self, event):
        try:
            AfcUci(self)
            PlaySound(self)
        except:
            BugReport(self)

    def import_factiva_xml(self,event):
        try :
            ImportFactiva(self, 'xml')
        except :
            BugReport(self)

    def import_factiva_mail(self, evt) :
        try :
            ImportFactiva(self, 'mail')
        except :
            BugReport(self)

    def import_factiva_txt(self, evt) :
        try :
            ImportFactiva(self, 'txt')
        except :
            BugReport(self)

    def OnImportTXM(self, evt) :
        try :
            ImportFactiva(self, 'txm')
        except :
            BugReport(self)

    def OnImportEuropress(self, evt) :
        try :
            ImportFactiva(self, 'euro')
        except :
            BugReport(self)

    def OnImportDMI(self, evt):
        ImportDMI(self, {})

    def OnExportMeta(self, evt, corpus = None):
        if corpus is None :
            corpus = self.tree.getcorpus()
        try :
            ExportMetaTable(self, corpus)
        except :
            BugReport(self)

    def ExtractTools(self, evt) :
        ID = evt.GetId()
        if ID == self.ID_splitvar :
            Extract(self, 'splitvar')
        elif ID == self.ID_extractmod :
            Extract(self, 'mods')
        elif ID == self.ID_extractthem :
            Extract(self, 'them')

    def OnTextReinert(self, event, corpus = None):
        try:
            #RunAnalyse(self, corpus, Alceste, OptAlceste)
            if corpus is None :
                corpus = self.tree.getcorpus()
            self.Text = Reinert(self, corpus, parametres = {'type': 'alceste'}, dlg = 6)
            if self.Text.val == wx.ID_OK:
                PlaySound(self)
        except:
            BugReport(self)

    def OnPamSimple(self, event, corpus = None):
        try:
            if corpus is None :
                corpus = self.tree.getcorpus()
            self.Text = AnalysePam(self, corpus, parametres = {'type' : 'pamtxt'}, dlg = 6)
            if self.Text.val == wx.ID_OK:
                PlaySound(self)
        except:
            BugReport(self)

    def SimiCluster(self, parametres = {}, fromprof = False, tableau = None) :
        self.analyse_matrix(DoSimi, parametres = parametres, analyse_type = 'simiclustermatrix', matrix = tableau, dlgnb = 5)

#    def OnSimi(self,evt):
#        try :
#            self.res = DoSimi(self, param = None)
            #self.res = Verges(self)
#            if self.res.val == wx.ID_OK :
#                PlaySound(self)
#        except :
#            BugReport(self)

    def OnHelp(self, event):
        webbrowser.open('http://www.iramuteq.org/documentation')

    def OnPref(self, event):
        dlg = PrefDialog(self)
        dlg.CenterOnParent()
        self.val = dlg.ShowModal()
        dlg.Destroy()

    def Upgrade(self) :
        if self.check_update:
            NewVersion(self)
        else:
            print('pas de verif')
        #IsNew(self)
        #CheckRPackages(self)

    def OnOpenFromCmdl(self):
        truepath = True
        if options.filename :
            if os.path.exists(options.filename):
                self.filename = os.path.abspath(options.filename)
            else:
                truepath = False
        elif args :
            if os.path.exists(os.path.realpath(args[0])):
                self.filename = os.path.abspath(os.path.realpath(args[0]))
            else:
                truepath = False
        else:
            return
        if truepath :
            if os.path.splitext(self.filename)[1] in ['.csv', '.xls', '.ods']:
                self.tableau = Tableau(self, self.filename)
                val = get_table_param(self, self.filename)
                if val == wx.ID_OK :
                    self.tableau.make_content()
                    OpenAnalyse(self, self.tableau.parametres)
                    self.tree.OnItemAppend(self.tableau.parametres)
                #get_table_param(self, self.filename)
                #self.tableau.make_content()
                #self.tableau.show_tab()
                #open_data(self, self.filename)
            elif os.path.splitext(self.filename)[1] == '.txt':
                self.OpenText()
            elif os.path.splitext(self.filename)[1] == '.ira' :
                #self.corpus = Corpus(self)
                #self.Text = OpenAnalyse(self, self.filename)
                OpenAnalyse(self, self.filename)
        if not truepath:
            print('This file does not exist')


#--------------------------------------------------------------------
# contenu de l'ecran d'accueil
# appelé seulement dans l'initialisation de IraFrame
#--------------------------------------------------------------------
class IntroPanel(wx.Panel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent)
        #col = randint(0, 255)
        #col1 = randint(0,255)
        #col2 = randint(0,255)
        #width, height = parent.GetSize()
        #print(width, height)
        #image_file = "/home/pierre/graph_emojiGJ.png"
        #bmp1 = wx.Image(image_file, wx.BITMAP_TYPE_ANY)
        #bmp1.Rescale(width, height)
        # image's upper left corner anchors at panel coordinates (0, 0)
        #self.bitmap1 = wx.StaticBitmap(self, -1, bmp1.ConvertToBitmap(), (0, 0))
        #col = 57
        col = 242
        col1 = 228
        col2 = 179
        bckgrdcolorhaut = wx.Colour(col, col1, col2)
        #bckgrdcolor = wx.Colour(randint(0, 255), randint(0, 255), randint(0, 255))
        self.SetBackgroundColour(bckgrdcolorhaut)
        txtcolour = wx.Colour(0, 0, 0)
        linkcolor = wx.Colour(255, 0, 0)
        sizer1 = wx.BoxSizer(wx.VERTICAL)
        sizer2 = wx.BoxSizer(wx.VERTICAL)
        sizer4 = wx.BoxSizer(wx.HORIZONTAL)
        grid_sizer_1 = wx.FlexGridSizer(1, 4, 0, 0)
        grid_sizer_3 = wx.FlexGridSizer(1, 4, 0, 0)
        grid_sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        iralink = hl.HyperLinkCtrl(self,
            wx.ID_ANY,
            "http://www.iramuteq.org",
            URL="http://www.iramuteq.org")
        iralink.SetColours(linkcolor, linkcolor, "RED")
        iralink.SetBackgroundColour(bckgrdcolorhaut)
        iralink.EnableRollover(True)
        iralink.SetUnderlines(False, False, True)
        iralink.SetBold(True)
        iralink.UpdateLink()
        PanelPres = wx.Panel(self)
        col = 70
        col1 = 176
        col2 = 70
        bckgrdcolor = wx.Colour(col, col1, col2)
        #bckgrdcolor = wx.Colour(randint(0, 255), randint(0, 255), randint(0, 255))
        PanelPres.SetBackgroundColour(bckgrdcolor)
        label_1 = wx.StaticText(self, -1, "IRaMuTeQ", size=(-1, -1))
        label_1.SetFont(wx.Font(46,
            wx.FONTFAMILY_TELETYPE,
            wx.FONTSTYLE_NORMAL,
            wx.FONTWEIGHT_BOLD,
            0,
            "Purisa"))
        label_1.SetForegroundColour(wx.RED)
        iraicone = wx.Image(os.path.join(ImagePath,'iraicone255x255.png'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        but_ira = wx.StaticBitmap(self, -1, bitmap = iraicone)
        label2 = wx.StaticText(PanelPres, -1 , '\nVersion ' + ConfigGlob.get('DEFAULT', 'version') + '\n')
        label2.SetForegroundColour(txtcolour)
        label2.SetBackgroundColour(bckgrdcolor)
        #self.hyper2 = hl.HyperLinkCtrl(PanelPres, wx.ID_ANY, "REPERE", URL="http://repere.no-ip.org/")
        #self.hyper2.SetColours(linkcolor, linkcolor, "RED")
        #self.hyper2.SetBackgroundColour(bckgrdcolor)
        #self.hyper2.EnableRollover(True)
        #self.hyper2.SetUnderlines(False, False, True)
        #self.hyper2.SetBold(True)
        #self.hyper2.UpdateLink()
        label_lerass = wx.StaticText(PanelPres, -1, 'Laboratoire ')
        label_lerass.SetForegroundColour(txtcolour)
        label_lerass.SetBackgroundColour(bckgrdcolor)
        self.hyper_lerass = hl.HyperLinkCtrl(PanelPres, -1, 'LERASS', URL="http://www.lerass.com")
        self.hyper_lerass.SetColours(linkcolor, linkcolor, "RED")
        self.hyper_lerass.SetBackgroundColour(bckgrdcolor)
        self.hyper_lerass.EnableRollover(True)
        self.hyper_lerass.SetUnderlines(False, False, True)
        self.hyper_lerass.SetBold(True)
        self.hyper_lerass.UpdateLink()
        blank = wx.StaticText(PanelPres, -1, '\n')
        blank1 = wx.StaticText(PanelPres, -1, '\n')
        labellicence = wx.StaticText(PanelPres, -1, _("License GNU GPL"))
        labellicence.SetForegroundColour(txtcolour)
        #labellicence.SetBackgroundColour(bckgrdcolor)
        labelcopy = wx.StaticText(PanelPres, -1, ConfigGlob.get('DEFAULT', 'copyright'))
        labelcopy.SetForegroundColour(txtcolour)
        #labelcopy.SetBackgroundColour(bckgrdcolor)
        python_img = wx.Image(os.path.join(ImagePath,'python-logo.jpg'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        r_img = wx.Image(os.path.join(ImagePath,'Rlogo.jpg'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        lexique_img = wx.Image(os.path.join(ImagePath,'LexTexte4.jpg'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        sms_img = wx.Image(os.path.join(ImagePath,'logo_sms.jpg'), wx.BITMAP_TYPE_ANY).ConvertToBitmap()
        but_python = wx.BitmapButton(self, -1, python_img)
        but_lexique = wx.BitmapButton(self, -1, lexique_img)
        but_r = wx.BitmapButton(self, -1, r_img)
        but_sms = wx.BitmapButton(self, -1, sms_img)
        self.Bind(wx.EVT_BUTTON, self.OnPython, but_python)
        self.Bind(wx.EVT_BUTTON, self.OnLexique, but_lexique)
        self.Bind(wx.EVT_BUTTON, self.OnR, but_r)
        self.Bind(wx.EVT_BUTTON, self.OnSMS, but_sms)
        #grid_sizer_1.Add(self.hyper2, 0, wx.EXPAND | wx.ALIGN_CENTER_HORIZONTAL, 0)
        grid_sizer_3.Add(label_lerass, 0, wx.EXPAND | wx.ALIGN_CENTER_HORIZONTAL, 0)
        grid_sizer_3.Add(self.hyper_lerass, 0, wx.EXPAND | wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer4.Add(label_1, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL, 5)
        sizer2.Add(label2, 0, wx.ALIGN_CENTER, 5)
        sizer2.Add(wx.StaticText(PanelPres, -1, ''), 0, wx.ALIGN_CENTER, 5)
        sizer2.Add(wx.StaticText(PanelPres, -1, ''), 0, wx.ALIGN_CENTER, 5)
        sizer2.Add(grid_sizer_3, 0, wx.ALIGN_CENTER, 5)
        sizer2.Add(wx.StaticText(PanelPres, -1, ' '), 0, wx.ALIGN_CENTER, 5)
        sizer2.Add(grid_sizer_1, 0, wx.ALIGN_CENTER, 5)
        sizer2.Add(labellicence, 0, wx.ALIGN_CENTER, 5)
        sizer2.Add(labelcopy, 0, wx.ALIGN_CENTER, 5)
        sizer1.Add(sizer4, 2, wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer1.Add(but_ira, 1, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL, 5)
        sizer1.Add(iralink, 1, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_TOP, 5)
        sizer2.Add(wx.StaticText(PanelPres, -1, ''), 0, wx.ALIGN_CENTER, 10)
        PanelPres.SetSizer(sizer2)
        grid_sizer_2.Add(but_python, 1, wx.ALIGN_CENTER_VERTICAL)
        grid_sizer_2.Add(but_lexique, 1, wx.ALIGN_CENTER_VERTICAL)
        grid_sizer_2.Add(but_r, 1,  wx.ALIGN_CENTER_VERTICAL)
        grid_sizer_2.Add(but_sms, 1,  wx.ALIGN_CENTER_VERTICAL)
        sizer1.Add(PanelPres, 0, wx.EXPAND |wx.ALL, 10)
        sizer1.Add(grid_sizer_2, 2, wx.ALIGN_CENTER_HORIZONTAL|wx.ALL, 1)
        self.SetSizer(sizer1)
        sizer1.Fit(self)

    def OnPython(self,evt):
        webbrowser.open('http://www.python.org')

    def OnLexique(self,evt):
        webbrowser.open('http://www.lexique.org')

    def OnR(self,evt):
        webbrowser.open('http://www.r-project.org')

    def OnSMS(self, evt):
        webbrowser.open('https://sms.univ-tlse2.fr')


#--------------------------------------------------------------------
# ecran d'accueil
# appelé seulement par MyApp
#--------------------------------------------------------------------
class MySplashScreen(wx.adv.SplashScreen):

    def __init__(self):
        bmp = wx.Image(os.path.join(ImagePath, 'splash.png')).ConvertToBitmap()
        wx.adv.SplashScreen.__init__(self, bmp,
            wx.adv.SPLASH_CENTRE_ON_SCREEN |
            wx.adv.SPLASH_TIMEOUT,
            2000,
            None,
            -1)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.fc = wx.CallLater(1, self.ShowMain)

    def OnClose(self, evt):
        evt.Skip()
        self.Hide()
        if self.fc.IsRunning():
            self.fc.Stop()
            self.ShowMain()

    def ShowMain(self):
        displaySize = wx.DisplaySize()
        w = displaySize[0]/1.2
        h = displaySize[1]/1.2
        frame = IraFrame(None, -1, "IRaMuTeQ " + ConfigGlob.get('DEFAULT', 'version'), size=(int(w), int(h)))
        frame.Show()
        frame.finish_init()
        frame.Upgrade()
        frame.OnOpenFromCmdl()
        #if self.fc.IsRunning():
        #    self.Raise()
        #wx.CallAfter(frame.ShowTip)


class MyApp(wx.App):

    def OnInit(self):
        """
        Create and show the splash screen.  It will then create and show
        the main frame when it is time to do so.
        """
        wx.SystemOptions.SetOption("mac.window-plain-transition", 1)
        self.SetAppName("Iramuteq")
        splash = MySplashScreen()
        splash.Show()
        return True

def main():
    app = MyApp(False)
    app.MainLoop()

if __name__ == '__main__':
    __name__ = 'Main'
    main()
