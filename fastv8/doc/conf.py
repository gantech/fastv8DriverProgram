# -*- coding: utf-8 -*-
import os
import sys

sys.path.append(os.path.abspath('../'))
sys.path.append(os.path.abspath('_themes/'))
sys.path.append(os.path.abspath('_extensions/'))

import better
import html_mods
#import latex_mods

extensions = [
              'fix_equation_ref',
              'sphinx.ext.intersphinx',
              'sphinxcontrib.bibtex',
              'sidenote',
              'sphinx.ext.mathjax',
              'sphinx_clatex',
              'context',
              'subfig',
              'marginfig',
              'numfig',
              'numsec',
              'figtable',
              'singlehtml_toc',
              'singletext',
              ]

templates_path = []
source_suffix = '.rst'
master_doc = 'index'

project = u'fastv8cppGlueCode'
copyright = u'2016 National Renewable Energy Laboratory'
# The short X.Y version.
version = better.__version__.split("-")[0]
# The full version, including alpha/beta/rc tags.
release = better.__version__
exclude_patterns = ['_build']
pygments_style = 'sphinx'

number_figures = True
numfig = False

html_use_smartypants = True
html_theme = 'better'
html_theme_options = {
    'inlinecss': """
    """,
    'nosidebar': True,
    'cssfiles': ['_static/forWeb.css'],
#    'scriptfiles': ['_static/testing.js'],
}
html_theme_path = [better.better_theme_path]
html_title = "" 
html_short_title = "Home"
html_sidebars = {
    '**': ['globaltoc.html'],
}

html_logo = None
html_favicon = None

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
html_show_sphinx = True
html_show_copyright = True
# Output file base name for HTML help builder.
htmlhelp_basename = 'Driver Program for FAST v8'

latex_additional_files = ['_static/csphinx.sty','_static/caesar.dtx','_static/caesar.ins','_static/caesar_book.cls']
clatex_documentclass = '\documentclass[oneside]{caesar_book}\n'
clatex_preamble = '\usepackage{mwe}\n \usepackage{csphinx}\n \usepackage[margin=8pt]{subcaption} \n \\captionsetup{labelfont=bf} \n \\title{FASTv8 C++ Glue Code}\n \\author{Ganesh Vijayakumar}\n \\publisher{National Renewable Energy Laboratory}'
clatex_user_chapters = False 
clatex_begin_doc = '\\frontmatter \n \\maketitlepage \n \\tableofcontents \n \\mainmatter \n'
context_documents = [
       ('index', 'fastv8cppGlueCode', u'FASTv8 C++ Glue Code',
        u'Ganesh Vijayakumar', 'manual'),
     ]


intersphinx_mapping = {'sphinx': ('http://sphinx-doc.org/', None)}


