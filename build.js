// Copyright (C) 2023 Alvaro Ramirez https://xenodium.com
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

const fs = require('fs');
const https = require('https');
const jsCode = fs.readFileSync('./dist/bundle.js', 'utf8');
const htmlContent = `
<!DOCTYPE html>
<html>
<head>
  <title>emacs.tv</title>
  <meta name="apple-mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="apple-touch-icon" href="icon.png">
  <style>
    body, div, p, h3 {
      font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      line-height: 1.5em;
    }
    .item p {
      margin: 0;
      display: flex;
      flex-wrap: wrap;
    }
    .item {
      margin-bottom: 1em;
    }
    .tag {
      color: #2A9D8F;
      cursor: pointer;
    }
    .dismissible {
      padding: 0.3em 0.6em;
      border: 1px solid #777;
      border-radius: 5px;
      font-size: 0.8em;
      margin: 0 0.2em;
      cursor: pointer;
    }
    .x {
      color: #777;
    }
    .speakers {
      color: #777;
      font-size: 0.8em;
    }
    a, a:visited {
      text-decoration: none;
      color: #4183C4;
    }
    .item .date {
      font-size: 0.95em;
      color: #777;
    }
  </style>
</head>
<body>
  <div id="root"></div>
  <script>
    document.addEventListener('DOMContentLoaded', async function () {
      store = app.makeStore();
      store.state.subscribe((state) => {
        const root = document.getElementById("root");
        if (!root) {
          return;
        }

        const { html, handlers } = app.render(state, store);
        root.innerHTML = html;

        handlers.forEach(({ nodeId, listenerName, handler }) => {
          const element = document.getElementById(nodeId);
          if (element) {
            element.addEventListener(listenerName, handler);
          }
        });
      });
     store.load();
    });
    ${jsCode}
  </script>
</body>
</html>
`.trim();

fs.writeFileSync('./index.html', htmlContent, 'utf8');
