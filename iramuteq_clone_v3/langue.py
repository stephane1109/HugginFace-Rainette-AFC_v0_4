# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os, sys
from configparser import ConfigParser, RawConfigParser
import gettext

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ConstructConfigPath, ConstructDicoPath, ConstructGlobalPath, PathOut

#------------------------------------
#pour le mki18n
#------------------------------------
def _init():
    global code_langues
    global langues
    global langueUI
    global preslangue

    # chemin de l'application
    AppliPath = os.path.abspath(os.path.dirname(os.path.realpath(sys.argv[0])))
    # preferences par defaut
    DefaultConf = ConfigParser()
    DictConfigPath = ConstructGlobalPath(AppliPath)
    ConfigGlob = ConfigParser()
    ConfigGlob.read(DictConfigPath['global'])
    DefaultConf.read(DictConfigPath['preferences'])
    # repertoire de l'utilisateur
    user_home = os.getenv('HOME')
    if user_home is None :
        user_home = os.path.expanduser('~')

    # chemin des préférences
    UserConfigPath = os.path.abspath(os.path.join(user_home, '.iramuteq-%s' % ConfigGlob.get('DEFAULT', 'version_nb')))
    ConfigPath = ConstructConfigPath(UserConfigPath)

    # dictionnaires des langues disponibles pour l'interface
    langues = {'french' : wx.LANGUAGE_FRENCH,
               'english' : wx.LANGUAGE_ENGLISH,
               'portuguese' : wx.LANGUAGE_PORTUGUESE,
               'italian' : wx.LANGUAGE_ITALIAN,
               'spanish' : wx.LANGUAGE_SPANISH
               }
    code_langues = {'french' : 'fr_FR',
               'english' : 'en',
               'portuguese' : 'pt_PT',
               'italian' : 'it_IT',
               'spanish' : 'es_ES'
               }
    # langue par defaut de l'interface (celle des messages écrits dans le code)
    langueUI = 'english'
    # dictionnaire des objets "traduction"
    preslangue = {}
    for langue in code_langues :
        preslangue[langue] = gettext.translation('iramuteq',\
            os.path.join(AppliPath,'locale'),\
            languages=[code_langues[langue]])
    ConfigPath = ConstructConfigPath(UserConfigPath)
    prefGlob = RawConfigParser()
    prefGlob.read(ConfigPath['preferences'])
    # guilanguage est l'appelation originale dans iramuteq, renommé en 'langageUI' dans le code en P3
    try :
        langueUI = prefGlob.get('iramuteq', 'guilanguage')
    except :
        langueUI = DefaultConf.get('iramuteq', 'guilanguage')

# la fonction appelée depuis tous les fichiers de module qui ont besoin du système de traduction
# elle appelle la fonction d'initialisation si il y a lieu

def run():
    try:
        langueUI
    except:
        _init()
    preslangue[langueUI].install()


#------------------------------------
# héritage de P2
#------------------------------------

# fonction présente dans iramuteq.py
# self fait référence à une IraFrame, dérivé de wx.Frame
#fonction conservée de l'ancienne version pour le cas ou, mais qui n'est pas utilisée en principe
def setlangue(self) :
    self.pref.read(self.ConfigPath['preferences'])
    try :
        langueUI = self.pref.get('iramuteq', 'guilanguage')
    except :
        langueUI = DefaultConf.get('iramuteq', 'guilanguage')
    self.preslangue.get(langueUI, 'english').install()
