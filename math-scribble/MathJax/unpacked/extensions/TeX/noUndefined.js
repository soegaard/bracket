/*************************************************************
 *
 *  MathJax/extensions/TeX/noUndefined.js
 *  
 *  This causes undefined control sequences to be shown as their macro
 *  names rather than producing an error message.  So $X_{\xxx}$ would
 *  display as an X with a subscript consiting of the text "\xxx".
 *  
 *  To configure this extension, use for example
 *  
 *      MathJax.Hub.Config({
 *        TeX: {
 *          noUndefined: {
 *            attributes: {
 *              mathcolor: "red",
 *              mathbackground: "#FFEEEE",
 *              mathsize: "90%"
 *            }
 *          }
 *        }
 *      });
 *
 *  ---------------------------------------------------------------------
 *  
 *  Copyright (c) 2010 Design Science, Inc.
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

//
//  The configuration defaults, augmented by the user settings
//  
MathJax.Extension["TeX/noUndefined"] = {
  config: MathJax.Hub.Insert({
    attributes: {
      mathcolor: "red"
    }
  },((MathJax.Hub.config.TeX||{}).noUndefined||{}))
};

MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
  var CONFIG = MathJax.Extension["TeX/noUndefined"].config;
  var MML = MathJax.ElementJax.mml;

  MathJax.InputJax.TeX.Parse.Augment({
    csUndefined: function (name) {
      this.Push(MML.mtext(name).With(CONFIG.attributes));
    }
  });

  MathJax.Hub.Startup.signal.Post("TeX noUndefined Ready");
});

MathJax.Ajax.loadComplete("[MathJax]/extensions/TeX/noUndefined.js");
