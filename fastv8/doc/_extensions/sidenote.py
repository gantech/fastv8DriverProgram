# 
# -*- coding: utf-8 -*-
import os
from docutils import nodes
from docutils.parsers.rst import roles
import string


class sidenote(nodes.Inline, nodes.TextElement):
    pass

def sidenote_role(typ, rawtext, text, lineno, inliner, options={}, content=[]):
    """Tufte style inline side note role for Sphinx."""

    role = sidenote() #Create an object of class sidenote

    text = text.decode('utf-8').encode('utf-8')
    # Handle sidenotes of the form "<tagName> SideNoteMessage"
    if '<' in text and '>' in text:
        tagName, sideNoteMessage = text.split('>')
        tagName = tagName.split('<')[1]
    else:
        tagName=''
        sideNoteMessage=''

    role['tag'] = tagName
    role['message'] = sideNoteMessage

    return [role], []  # Have to return an object of type sidenote which inherits node

def html_visit_sidenote(self, node):
    """ Generate HTML for sidenote """
    self.body.append('<label for="sn-{0}" class="margin-toggle sidenote-number"></label><span class="sidenote">{1}'.format(node['tag'], node['message']) )

def html_depart_sidenote(self, node):
    """ Generate HTML for sidenote """
    self.body.append('</span>')

def latex_visit_sidenote(self, node):
    """ Generate LaTeX code for sidenote """
    self.body.append("\sidenote{{{0}}} ".format(node['message']))

def latex_depart_sidenote(self, node):
    """ Generate LaTeX code for sidenote """
    self.body.append('')

def setup(app):
    app.add_node(sidenote, override=True,
        html=(html_visit_sidenote, html_depart_sidenote),
        latex=(latex_visit_sidenote, latex_depart_sidenote))
    app.add_role('sidenote', sidenote_role)


if __name__=="__main__":
    print sideNotehtml('Jon13','Jason M. Jonkman. The New Modularization Framework for the FAST Wind Turbine CAE Tool. Technical Report NREL/CP-5000-57228, National Renewable Energy Laboratory, January 2013.')
