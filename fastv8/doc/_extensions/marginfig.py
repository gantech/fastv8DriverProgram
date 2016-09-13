"""
Adds margin figure functionality
"""

from docutils import nodes
import docutils.parsers.rst.directives as directives
from sphinx.util.compat import Directive
from sphinx import addnodes

class marginfig(nodes.General, nodes.Element):
    pass

def skip_visit(self, node):
    raise nodes.SkipNode

def visit_marginfig_tex(self, node):
    pass

def depart_marginfig_tex(self, node):
    pass

def visit_marginfig_html(self, node):
    pass

def depart_marginfig_html(self, node):
    pass

class marginfigstart(nodes.General, nodes.Element):
    pass

def visit_marginfigstart_tex(self, node):
    self.body.append('\n\\begin{marginfigure}\n\n')

def depart_marginfigstart_tex(self, node):
    pass

def visit_marginfigstart_html(self, node):
    self.body.append('<span class="marginfigure">')    

def depart_marginfigstart_html(self, node):
    pass

class marginfigend(nodes.General, nodes.Element):
    pass

def visit_marginfigend_tex(self, node):
    pass

def depart_marginfigend_tex(self, node):
    self.body.append('\n\n\\end{marginfigure}\n\n')

def visit_marginfigend_html(self, node):
    pass

def depart_marginfigend_html(self, node):
    self.body.append('</span>')

class MarginfigEndDirective(Directive):
    has_content = False
    optional_arguments = 0

    def run(self):
        node = marginfigend()
        return [node]

class MarginfigStartDirective(Directive):
    has_content = False
    optional_arguments = 0

    def run(self):
        node = marginfigstart()
        return [node]

def setup(app):
    app.add_node(marginfigstart,
                 html=(visit_marginfigstart_html, depart_marginfigstart_html),
                 singlehtml=(visit_marginfigstart_html, depart_marginfigstart_html),
                 text=(skip_visit, None),
                 latex=(visit_marginfigstart_tex, depart_marginfigstart_tex))

    app.add_node(marginfig,
                 html=(visit_marginfig_html, depart_marginfig_html),
                 singlehtml=(visit_marginfig_html, depart_marginfig_html),
                 text=(skip_visit, None),
                 latex=(visit_marginfig_tex, depart_marginfig_tex))

    app.add_node(marginfigend,
                 html=(visit_marginfigend_html, depart_marginfigend_html),
                 singlehtml=(visit_marginfigend_html, depart_marginfigend_html),
                 text=(skip_visit, None),
                 latex=(visit_marginfigend_tex, depart_marginfigend_tex))

    app.add_directive('marginfigstart', MarginfigStartDirective)
    app.add_directive('marginfigend', MarginfigEndDirective)


