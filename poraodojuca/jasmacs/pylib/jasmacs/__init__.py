# -*- coding: utf-8 -*-
# Copyright 2018 Juca Crispim <juca@poraodojuca.net>

# This file is part of jasmacs.

# jasmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# jasmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with jasmacs. If not, see <http://www.gnu.org/licenses/>.

import mimetypes
import pkg_resources
import sys
import cherrypy
from jasmine_core.core import Core
from jasmine.config import Config
from jasmine.entry_points import Command
from jasmine.standalone import JasmineApp
from jasmine.ci import CIRunner

html_theme = 'clear'

class JasmacsApp(JasmineApp):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.jasmine_file = JasmacsFile()


class JasmacsCore(Core):

    @classmethod
    def css_package(cls):
        return __package__

    @classmethod
    def js_package(cls):
        return __package__


    @classmethod
    def css_files(cls, theme='dark'):
        css_filter = theme + '.css'
        return cls._uniq(sorted(filter(
            lambda x: css_filter in x, pkg_resources.resource_listdir(
                cls.css_package(), ''))))

    @classmethod
    def js_files(cls):
        js_files = sorted(list(filter(
            lambda x: '.js' in x, pkg_resources.resource_listdir(
                cls.js_package(), ''))))

        # jasmine.js needs to be first
        js_files.insert(0, 'jasmine.js')

        # boot needs to be last
        js_files.remove('boot.js')
        js_files.append('boot.js')

        return cls._uniq(js_files)



class JasmacsConfig(Config):

    def stylesheet_urls(self):
        global html_theme

        jasmine_css = [
            "/__jasmine__/{0}".format(core_css)
            for core_css in JasmacsCore.css_files(theme=html_theme)]

        src_css = [
            "/__src__/{0}".format(css_file) for css_file in self.stylesheets()]

        return jasmine_css + src_css

    def script_urls(self):
        core_js_files = JasmacsCore.js_files()
        if 'node_boot.js' in core_js_files:
            core_js_files.remove('node_boot.js')

        jasmine_js = [
            "/__jasmine__/{0}".format(core_js) for core_js in core_js_files]

        src_js = [self._prefix_src_underscored(src_file)
                  for src_file in self.src_files()]

        helpers = ["/__spec__/{0}".format(helper) for helper in self.helpers()]

        specs = ["/__spec__/{0}".format(spec_file)
                 for spec_file in self.spec_files()]
        return jasmine_js + src_js + helpers + specs



class JasmacsCommand(Command):

    def _load_config(self, custom_config_path):
        config_file, project_path = self._config_paths(custom_config_path)
        return JasmacsConfig(config_file, project_path=project_path)


class JasmacsFile(object):
    @cherrypy.expose
    def index(self, path):
        if path == 'jasmine_favicon':
            return cherrypy.lib.static.serve_fileobj(
                pkg_resources.resource_stream(
                    'jasmine_core.images',
                    'jasmine_favicon.png'
                ),
                content_type='image/png'
            )
        mime_type, _ = mimetypes.guess_type(path)
        return cherrypy.lib.static.serve_fileobj(
            pkg_resources.resource_stream('jasmacs', path),
            content_type=mime_type
        )


def begin():
    global html_theme

    if '--dark-theme'in sys.argv:
        html_theme = 'dark'
        sys.argv.remove('--dark-theme')

    cmd = JasmacsCommand(JasmacsApp, CIRunner)
    cmd.run(sys.argv[1:])



if __name__ == '__main__':
    begin()
