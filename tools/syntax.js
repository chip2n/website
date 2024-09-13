const fs = require('fs');
const Prism = require('./prism.js');

const code = fs.readFileSync(process.argv[2], 'utf-8')
const html = Prism.highlight(code, Prism.languages.lisp, 'lisp');

process.stdout.write(html);
