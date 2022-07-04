import * as THREE from 'three';
import { OrbitControls } from 'OrbitControls';
import { setCookie, getCookie, eraseCookie, isMobile, getParams } from '/utils.js'

WsIrcClient.default_username = 'matt.one'
WsIrcClient.default_nick_prefix = 'mtk-'

var scene = null;
var planets = []

function load3dscene(full) {
  scene = new THREE.Scene();
  var camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight);
  var renderer = new THREE.WebGLRenderer({
    canvas: document.querySelector("#bg")
  })
  let controls = null;
  if (full) {
    controls = new OrbitControls(camera, document.querySelector('html'))
  } else {
    controls = new OrbitControls(camera, renderer.domElement)
  }

  window.addEventListener('resize', onWindowResize, false);

  function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setPixelRatio(window.devicePixelRatio)
    renderer.setSize(window.innerWidth, window.innerHeight)
  }
  onWindowResize()

  camera.position.setZ(10)
  const initialPos = Object.assign({}, camera.position)

  function addPlanet(size, name, rotation, update) {
    const planetTexture = new THREE.TextureLoader().load('/assets/' + name + ".jpg")
    const planet = new THREE.Mesh(
      new THREE.SphereGeometry(size, 32, 32),
      new THREE.MeshStandardMaterial({
        map: planetTexture,
      })
    )
    scene.add(planet)
    planets.push({ planet, rotation, update, radius: size })
    return planet
  }

  addPlanet(3, "earth", [0, 0.001, 0])
  addPlanet(1, "moon", [0, 0.002, 0], (p) => {
    const radius = 15
    const cos = Math.cos(Date.now() / 10000)
    const sin = Math.sin(Date.now() / 10000)
    p.x = -cos * radius
    p.z = sin * radius
  })
  addPlanet(3, "mars", [0, 0.001, 0]).position.set(60, 0, -60)
  const jupiter = addPlanet(33, "jupiter", [0, -0.0005, 0])
  jupiter.position.set(80, 50, 400)
  jupiter.rotateY(Math.PI)

  // Sun
  const pointLight = new THREE.PointLight(0xfffffff, 1)
  pointLight.position.set(-20, 0, 20)
  scene.add(pointLight)

  const ambientLight = new THREE.AmbientLight(0xfffffff, 0.2)
  scene.add(ambientLight)


  const starColors = [0xffffff, 0xffaaff, 0xaaaaff, 0xffaaaa]
  function addStar() {
    const geometry = new THREE.SphereGeometry(0.15, 24, 24)
    const color = starColors[Math.floor(Math.random() * starColors.length)]
    const material = new THREE.MeshPhongMaterial({ color, emissive: color, emissiveIntensity: 20.0 })
    const star = new THREE.Mesh(geometry, material)

    const [x, y, z] = Array(3).fill().map(() => THREE.MathUtils.randFloatSpread(500))
    star.position.set(x, y, z)
    scene.add(star)
  }

  Array(1000).fill().forEach(addStar)

  const spaceTexture = new THREE.TextureLoader().load('/assets/nebula.png')
  spaceTexture.wrapS = THREE.RepeatWrapping
  spaceTexture.wrapT = THREE.RepeatWrapping
  spaceTexture.repeat.set(1, 0.5);
  scene.background = spaceTexture;


  function scrollAnimate() {
    if (scene === null) return;
    const t = document.body.getBoundingClientRect().top
    scene.background.offset.y = t * +0.00007
    camera.position.z = t * 0.02 + initialPos.z;
    camera.position.x = t * 0.02 + initialPos.x;
    camera.position.y = t * 0.02 + initialPos.y;
  }

  document.body.onscroll = scrollAnimate;


  function animate() {
    if (scene === null) {
      return
    }
    setTimeout(function () {
      requestAnimationFrame(animate);
    }, 1000 / 30);

    renderer.render(scene, camera)

    planets.forEach((p) => {
      p.planet.rotation.x += p.rotation[0];
      p.planet.rotation.y += p.rotation[1];
      p.planet.rotation.z += p.rotation[2];
      if (p.update) {
        p.update(p.planet.position)
      }
    })

    controls.update();
  }

  animate()
}


function disposeScene() {
  if (!scene) {
    return
  }
  scene.remove.apply(scene, scene.children);
  scene.clear()
  setTimeout(function () {
    scene = null
    window.dispatchEvent(new Event('resize'));
  }, 300);
}

function reloadScene(...params) {
  disposeScene()
  setTimeout(function () {
    load3dscene(params)
  }, 1000)
}

const toggler = document.querySelector("#disable3d")

function disablefx() {
  disposeScene()
  document.documentElement.style.setProperty('--spacing', "10px");
  document.querySelectorAll(".title").forEach((x) => {
    x.style.setProperty("-webkit-animation", "")
    x.style.setProperty("-moz-animation", "")
    x.style.setProperty("animation", "")
  })
}

function enablefx() {
  load3dscene()
  toggler.checked = true
  document.documentElement.style.setProperty('--spacing', "150px");
  document.querySelectorAll(".title").forEach((x) => {
    x.style.setProperty("-webkit-animation", "glow 1s ease-in-out infinite alternate")
    x.style.setProperty("-moz-animation", "glow 1s ease-in-out infinite alternate")
    x.style.setProperty("animation", "glow 1s ease-in-out infinite alternate")
  })
}


function toggle3d() {
  setCookie('load3d', String(toggler.checked));
  if (toggler.checked) {
    enablefx()
  } else {
    disablefx()
  }
}

toggler.onclick = toggle3d

// on page load
var load3d = getCookie('load3d');
// if ((isMobile && (!load3d || load3d !== "true")) || (load3d && load3d === "false")) {
if ((!load3d || load3d !== "true") || (load3d && load3d === "false")) {
  toggler.checked = false
} else {
  enablefx()
}

if (isMobile) {
  document.querySelector(".extraNav").style.display = "none"
  document.querySelector("#postamble .rightNav").style.display = "block"
}


// TERMINAL

//load sitemap for ls and cd commands
let hasLoadedContent = false;

document.addEventListener("DOMContentLoaded", async function () {
  if (hasLoadedContent) return;
  hasLoadedContent = true

  let sitemap = await (await fetch("/sitemap.json")).json();
  sitemap = sitemap[0].contents;

  let wd = [".", ...window.location.pathname.toString().split("/").slice(0, -1).filter((p) => p)]

  function getDirObj(dirlist, i = 1, cwd = null) {
    cwd = cwd === null ? sitemap : cwd
    if (i > dirlist.length) {
      console.error(`dirlist is invalid: ${dirlist}`)
      return cwd
    }
    for (const node of cwd) {
      if (node.name === dirlist[i] && node.type === "directory") {
        return getDirObj(dirlist, i + 1, node.contents)
      }
    }
    return cwd
  }

  function getcwd() {
    return getDirObj(wd)
  }

  function getNodeType(type, cwd) {
    const nodes = []
    cwd.forEach((node) => {
      if (node.type === type) {
        nodes.push(node.name)
      }
    })
    return nodes
  }

  function getWdNodeType(type) {
    let cwd = getcwd()
    return getNodeType(type, cwd)
  }

  const commands = {
    "search": () => { },
    "pwd": () => { },
    "ls": () => { },
    "cd": () => { return getWdNodeType("directory") },
    "open": () => { return getWdNodeType("file") },
    "ping": () => { },
    "apt": () => { },
    "pacman": () => { },
    "gasconheart": () => { },
    "explore": () => { },
    "torus": () => { },
    "sphere": () => { },
    "cat": () => { },
    "star": () => { },
    "follow": () => { },
    "cookieclean": () => { },
    "python": () => { },
    "irc": () => { },
    "lisp": () => { },
    "js": () => { },
    "why": () => { }
  }

  var animation = false;
  var prompt;
  var string;

  function progress(percent, width) {
    var size = Math.round(width * percent / 100);
    var left = '', taken = '', i;
    for (i = size; i--;) {
      taken += '=';
    }
    if (taken.length > 0) {
      taken = taken.replace(/=$/, '>');
    }
    for (i = width - size; i--;) {
      left += ' ';
    }
    return '[' + taken + left + '] ' + percent + '%';
  }

  const pyIinterpreter = window.jspython.jsPython();
  var pyscript = "";

  (async function ($) {
    $('#terminal').terminal({
      help: function () {
        this.echo(`
  THE AVAILABLE COMMANDS ARE
  ----------------------------

  search: Search string over the blog (Put it in between quotes)
  pwd: Print working directory
  ls: List directory
  cd: Move to a directory
  open: Open a webpage from the current directory passed as argument
  cat: cat
  star: star this project on github
  follow: follow me on github
  explore: Explore the 3d space (requires FX enabled)
  torus: Replaces the planets with donuts (Reveal the truth)
  sphere: Comes back to NASA's fake spherical model of the planets
  cookieclean: Removes the cookies you have from this website.
  python: Launch python shell!
  js: Launch javascript interpreter
  lisp: Launch lisp interpreter
  irc: Chat on our irc!
  help: shows this help menu
  `);
      },
      search: async function (query) {

        async function loadPage(path) {
          const page = await fetch(path)
          const html = await page.text()
          const doc = new DOMParser().parseFromString(html, "text/html")
          return [doc.title, doc.querySelector("main").textContent]
        }

        function getDirs(smap) {
          let dirs = smap.filter(p => p.type === "directory")
          return dirs
        }

        function allFiles(dir = null, smap = null) {
          smap = smap === null ? sitemap : smap
          dir = dir === null ? ["."] : dir
          let files = [...smap.filter(p => p.type === "file").map(p => ("/" + dir.slice(1).join("/") + "/" + p.name).replace(/\/+/g, '/'))]
          for (const dobj of getDirs(smap)) {
            let d = dobj.name
            let ndir = [...dir, d]
            let nmap = dobj.contents
            files = [...files, ...allFiles(ndir, nmap)]
          }
          return files
        }

        this.echo(`Searching for:  '${query}'. CTRL+C to cancel`)

        let i = 0;
        let size = 50
        let term = this;
        const all = allFiles()
        const len = all.length
        prompt = this.get_prompt();
        string = progress(0, size);
        this.set_prompt(progress);
        animation = true;

        // TODO animation not displaying
        for (const file of all) {
          if (animation === false) {
            return
          }
          string = progress(100 * i / len, size);
          term.set_prompt(string);
          let [title, text] = await loadPage(file)
          if (text.toLocaleLowerCase().includes(query.toLocaleLowerCase())) {
            term.echo($(`<a href="${file}">${title}<a/><br>`))
          }
          i++;
        }
        term.echo(progress(100, size) + ' [[b;green;]FINISHED]')
          .set_prompt(prompt);
        animation = false
      },
      pwd: function () {
        const title = document.title
        const path = window.location.pathname.toString()
        const wds = "/" + wd.slice(1).join("/") + "/"
        this.echo($(`<p>${path} <a href="${path}">${title}<a/><p/><p>${wds}</p>`));
      },
      ls: function () {
        let names = "<p>"
        const wds = "/" + wd.slice(1).join("/") + "/"
        let folders = getWdNodeType("directory")
        let files = getWdNodeType("file")
        files.forEach((name) => {
          const url = (wds + name).replace(/\/+/g, '/')
          names += `<a href="${url}">${name}</a><br>`
        })
        folders.forEach((name) => {
          names += ` ${name}<br>`
        })
        names += "</p>"
        this.echo($(names))
      },
      cd: function (path) {
        if (path == "..") {
          if (wd.length > 1) wd.pop()
          else this.echo("No upper directory!")
          return
        }
        let folders = getWdNodeType("directory")
        let files = getWdNodeType("file")
        if (files.includes(path)) {
          this.echo(`'${path}' is a webpage, you can open with with 'open'!`);
          return
        }
        if (!folders.includes(path)) {
          this.echo(`'${path}' is not a valid directory!`);
          return
        }
        wd.push(path)
      },
      open: function (file) {
        let files = getWdNodeType("file")
        if (!files.includes(file)) {
          this.echo(`'${file}' is not a valid webpage!`);
          return
        }
        this.echo("Oppened in a new tab!")
        const wds = "/" + wd.slice(1).join("/") + "/"
        const url = (wds + file).replace(/\/+/g, '/')
        window.open(url)
      },
      ping: function () {
        this.echo("pong\n")
      },
      apt: function () {
        this.echo(`
 ______
< what >
 ------
        \\   ^__^
         \\  (oo)\\_______
            (__)\\       )\\/\\
                ||----w |
                ||     ||

    `)
      },
      pacman: function () {
        this.echo(`
================================================.
     .-.   .-.     .--.                         |
    | OO| | OO|   / _.-' .-.   .-.  .-.   .''.  |
    |   | |   |   \\  '-. '-'   '-'  '-'   '..'  |
    '^^^' '^^^'    '--'                         |
===============.  .-.  .================.  .-.  |
               | |   | |                |  '-'  |
               | |   | |                |       |
               | ':-:' |                |  .-.  |
l42            |  '-'  |                |  '-'  |
==============='       '================'       |
    `)
      },
      gasconheart: function () {
        this.echo("nc mangle.ga 8888")
      },
      explore: function () {
        document.querySelector("main").remove()
        document.querySelector("#postamble").remove()
        document.querySelector(".title").remove()
        document.querySelector("#disable3dlabel").remove()
        document.querySelector("#bgcover").remove()
        document.querySelector("#comments").remove()
        reloadScene({ path: true })
        this.echo("I've hidden he boring stuff that was written here\nYou might want to close this window for now or type 'help' to see new commands");
      },
      torus: function () {
        planets.forEach((p) => {
          if (p.planet.geometry.type === "TorusGeometry") {
            return
          }
          const radius = p.radius
          p.planet.geometry.dispose();
          const geometry = new THREE.TorusGeometry(3 * radius, radius, 16, 100)
          p.planet.geometry = geometry
          p.rotation[2] = p.rotation[1] * 2
        })
      },
      sphere: function () {
        planets.forEach((p) => {
          if (p.planet.geometry.type === "SphereGeometry") {
            return
          }
          const radius = p.radius
          p.planet.geometry.dispose();
          const geometry = new THREE.SphereGeometry(radius, 32, 32)
          p.planet.geometry = geometry
          p.rotation[2] = 0
        })
      },
      cookieclean: function () {
        eraseCookie("load3d")
        eraseCookie("username")
        document.getElementById("comment-name").value = ""
      },
      cat: function () {
        this.echo($('<img src="https://placekitten.com/408/287">'));
      },
      star: function () { this.echo($('<iframe src="https://ghbtns.com/github-btn.html?user=matheusfillipe&repo=myblog&type=star&count=true&size=large" frameborder="0" scrolling="0" width="170" height="30" title="GitHub"></iframe>')) },
      follow: function () { this.echo($('<iframe src="https://ghbtns.com/github-btn.html?user=matheusfillipe&type=follow&count=true&size=large" frameborder="0" scrolling="0" width="230" height="30" title="GitHub"></iframe>')) },

      js: function () {
        this.push(function (cmd, term) {
          try {
            if (cmd == 'exit') {
              term.pop();
              return;
            }
            var result = window.eval(cmd);
            if (result != undefined) {
              term.echo(String(result));
            }
          } catch (e) {
            term.echo("[[gb;red;]" + e + "]")
          }

        }, {
          prompt: 'js> ',
          name: 'js',
          completion: () => ["exit"]
        });
      },
      lisp: function () {
        this.echo("Loading LIPS REPL....");
        setTimeout(() => {
          document.querySelector("#terminalwindow").classList.add("terminal--hidden")
        }, 2000);
        (function (next) {    /**     * This is bookmarklet that will create terminal with LIPS REPL     *     * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl/me>     * Released under MIT license     */    var orig_jQuery; var dolar; if (window.jQuery) { if (window.$ === window.jQuery) { dolar = true; } orig_jQuery = window.jQuery.noConflict(true); } function attr(elem, key, value) { elem.setAttribute(document.createAttribute(key, value)); } var script = (function () { var head = document.getElementsByTagName('head')[0]; return function (src) { var script = document.createElement('script'); script.setAttribute('src', src); script.setAttribute('type', 'text/javascript'); head.appendChild(script); return script; }; })(); script('https://cdn.jsdelivr.net/npm/jquery'); (function delay(time) { if (typeof jQuery == 'undefined') { setTimeout(delay, time); } else { next(jQuery, function () { if (orig_jQuery) { window.jQuery = orig_jQuery; if (dolar) { window.$ = window.jQuery; } } }); } })(500); })(async function ($, next) { async function hash(branch) { try { var url = `https://api.github.com/repos/jcubic/lips/commits?sha=${branch}`; var res = await fetch(url); var data = await res.json(); return data[0].sha; } catch (e) { return branch; } } const REF = await hash('master'); function init() { var t = $('.terminal.lips'); if (t.length) { t.each(function () { $(this).terminal().destroy().remove(); }); } $.terminal.defaults.linksNoReferrer = true; $.terminal.defaults.formatters = $.terminal.defaults.formatters.filter((x) => { return x.name !== 'syntax_scheme'; }); $.terminal.syntax("scheme"); $('.shell-wrapper').remove(); var wrapper = $('<div>').addClass('shell-wrapper').appendTo('body'); var container = $('<div>').addClass('shell-container').appendTo(wrapper); var mask = $('<div class="shell-mask"/>').appendTo(wrapper); var nav = $('<nav/>').appendTo(container); var pos; $(document).off('mousemove'); var height; $('nav').off('mousedown').mousedown(function (e) { height = container.height(); pos = e.clientY; wrapper.addClass('drag'); return false; }); $(document).off('.terminal').on('mousemove.terminal', function (e) { if (pos) { container.height(height + (pos - e.clientY)); } }).on('mouseup.terminal', function () { pos = null; wrapper.removeClass('drag'); }); $('<span class="shell-destroy">[x]</span>').click(function () { term.destroy(); wrapper.remove(); }).appendTo(nav); var term = terminal({ selector: $('<div class="lips">').appendTo(container), name: 'lips', lips }); if (typeof lips.env.get('write', { throwError: false }) === 'undefined') { var path = `https://cdn.jsdelivr.net/gh/jcubic/lips@${REF}/`; term.exec(['(let ((e lips.env.__parent__))', '(load "' + path + 'dist/std.xcb" e))'].join('\n'), true); } function format_baner(banner) { return banner.replace(/^[\s\S]+(LIPS.*\nCopy.*\n)[\s\S]*/, '$1').replace(/(Jakub T. Jankiewicz)/, '[[!;;;;https://jcubic.pl/me]$1]'); } term.echo(format_baner(lips.banner), { formatters: false }); next(); } ['https://cdn.jsdelivr.net/gh/jcubic/jquery.terminal/css/jquery.terminal.min.css', 'https://cdn.jsdelivr.net/gh/jcubic/lips@devel/lib/css/terminal.css', 'https://cdn.jsdelivr.net/gh/jcubic/terminal-prism/css/prism-coy.css'].forEach(function (url) { if (!$('link[href="' + url + '"]').length) { var link = $('<link href="' + url + '" rel="stylesheet"/>'); var head = $('head'); if (head.length) { link.appendTo(head); } else { link.appendTo('body'); } } }); if (typeof $.terminal !== 'undefined') { init(); } else { var scripts = ['https://cdn.jsdelivr.net/npm/prismjs/prism.js', ['https://cdn.jsdelivr.net/combine/npm/jquery.terminal', 'npm/jquery.terminal/js/prism.js', 'npm/prismjs/components/prism-scheme.min.js', `gh/jcubic/lips@${REF}/lib/js/terminal.js`, `gh/jcubic/lips@${REF}/lib/js/prism.js`, 'npm/js-polyfills/keyboard.js'].join(','), 'https://cdn.jsdelivr.net/npm/browserfs@1.x.x/dist/browserfs.min.js']; (function recur() { var script = scripts.shift(); if (!script) { if (window.lips) { init(); } else { $.getScript(`https://cdn.jsdelivr.net/gh/jcubic/lips@${REF}/dist/lips.min.js`, init); } } else if (script.match(/prism.js$/) && typeof Prism !== 'undefined') { recur(); } else { $.getScript(script, recur); } })(); } });

      },
      python: function () {
        this.terminal().push(function (cmd, term) {
          if (cmd == 'help') {
            term.echo($(`<p>This is a python shell using <a href="https://www.jspython.dev">jspython<a></p>`));
          } else if (cmd == 'exit') {
            pyscript = "";
            term.pop();
          } else if (!cmd) {
            return
          } else {
            pyIinterpreter
              .evaluate(pyscript + cmd)
              .then(res => {
                this.echo(res)
                pyscript += cmd + "\n"
              })
              .catch((e) => this.echo("[[gb;red;]" + e + "]"));
          }
        }, {
          prompt: 'python> ',
          name: 'python',
          completion: () => ["exit"]
        });
      },
      irc: function () {
        let term = this.terminal()
        term.echo("Connecting to irc...")
        term.echo()

        let irc_channel = "#romanian"
        let irc_in_channels = new Set()

        let irc_client = new WsIrcClient({
          server: "irc.dot.org.es",
          port: 7669,
          is_ssl: true,
          // debug: true
        })
          .onmessage(function (channel, from, message) {
            term.echo("[[gb;blue;]" + `${channel} -> ${from}: ] ${message}`)
          })
          .onconnect(function () {
            term.echo($(`<p>Welcome to <a href="https://irc.dot.org.es">irc.dot.org.es</a>!</p>`));
            this.join(irc_channel)
          })
          .onjoin(function (channel) {
            term.echo(`Joined ${channel}`)
            irc_channel = channel
            term.set_prompt(`${irc_channel}> `)
            irc_in_channels.add(irc_channel)
          })
          .onpart(function (channel) {
            term.echo(`Leaving ${channel}`)
            irc_in_channels.delete(channel)
            if (irc_channel === channel) {
              irc_channel = Array.from(irc_in_channels)[0]
              term.set_prompt(`${irc_channel}> `)
            }
          })
          .onnames(function (channel, names) {
            term.echo("[[gb;blue;]" + channel + " -] Names: " + names.join(" "))
          })
          .onnickinuse(function () {
            term.echo("[[gb;red;]" + "That nickname is already in use!" + "]")
          })
          .onnickchange(function (nick) {
            term.echo(`You are now know as: ${nick}`)
          })
          .connect()


        let irc_commands = ["/quote", "/send", "/part", "/mode", "/kick", "/topic", "/names"]
        this.terminal().push(function (cmd, term) {

          if (cmd.trim().length == 0) return;
          if (irc_channel === undefined) term.echo("[[gb;red;]You are not in any channel. Type /join #romanian]");

          if (!cmd.startsWith("/")) {
            irc_client.send(irc_channel, cmd)
            // term.echo("[[gb;orange;]" + `${irc_channel} - YOU:] ${cmd}`)
            return

          } else if (irc_commands.includes(cmd.split(" ")[0])) {
            let m_name = cmd.split(" ")[0].slice(1)
            let method = irc_client[m_name]
            let user_args = cmd.split(/\s+/).slice(1)
            let arg_names = getParams(method)
            if (user_args.length < method.length) {
              term.echo("[[gb;red;]" + `This command takes at least ${method.length} arguments: ${arg_names.join(", ")}` + "]")
              return
            }

            let args = []
            if (user_args.length > method.length) {
              let last = []
              for (const arg of user_args) {
                if (args.length < method.length - 1) {
                  args.push(arg)
                } else {
                  last.push(arg)
                }
              }
              console.log(args)
              args.push(last.join(" "))
            } else {
              args = user_args
            }

            method.bind(irc_client)(...args)
            return
          }

          switch (cmd.split(/\s+/)[0]) {

            case "/nick":
              if (cmd.split(/\s+/).length < 2) {
                term.echo("[[gb;red;]" + "Pass your desired nickname as an argument" + "]")
                return
              }
              irc_client.set_nick(cmd.split(/\s+/)[1])
              break

            case "/join":
              if (cmd.split(/\s+/).length < 2) {
                term.echo("[[gb;red;]" + "Pass your desired nickname as an argument" + "]")
                return
              }
              let channel = cmd.split(/\s+/)[1]
              if (irc_in_channels.has(channel)) {
                irc_channel = channel
                term.set_prompt(`${irc_channel}> `)
              } else {
                irc_client.join(channel)
              }
              break

            case '/help':
              term.echo($(`<p>Welcome to irc.dot.org.es!</p>`));
              term.echo("THIS IS A SIMPLE WEB TERMINAL IRC CLIENT")
              term.echo()
              term.echo("            COMMANDS:  ")
              for (const command of [...irc_commands, "/nick", "/join", "/quit"]) {
                term.echo(command)
              }
              break

            case '/quit':
              console.log("Closing irc...")
              irc_client.close()
              term.pop();
              break

            default:
              term.echo(' [[b;red;]Invalid command!]')
          }
        }, {
          prompt: `${irc_channel}> `,
          name: 'irc',
          completion: () => ["/quit", "/help", "/nick", "/join", ...irc_commands]
        });
      },
      why: function () { this.echo("Why not?") }
    }, {
      greetings: `
  ______                    _             __
 /_  __/__  _________ ___  (_)___  ____ _/ /
  / / / _ \\/ ___/ __ \`__ \\/ / __ \\/ __ \`/ /
 / / /  __/ /  / / / / / / / / / / /_/ / /
/_/  \\___/_/  /_/ /_/ /_/_/_/ /_/\\__,_/_/


  WELCOME TO THE TERMINAL
  Type 'help' to see available commands
  `,
      enabled: false,
      completion: function (string, callback) {
        for (const cmd of Object.keys(commands)) {
          const pattern = new RegExp("^" + cmd)
          if (this.get_command().match(pattern)) {
            const cmds = commands[cmd]();
            return cmds
          }
        }
        const cmds = Array.from(Object.keys(commands));
        return cmds
      },
      keydown: function (e, term) {
        if (e.which === 68 && e.ctrlKey) { // CLTR+D
          document.querySelector("#terminalwindow").classList.add("terminal--hidden")
          document.activeElement.blur();
          return false;
        }
        if (animation) {
          if (e.which == 67 && e.ctrlKey) { // CTRL+C
            animation = false;
            term.echo(string + ' [[b;red;]FAIL]')
              .set_prompt(prompt);
          }
          return false;
        }
      }
    });
    $('#terminal').terminal().echo(new $.terminal.FramesAnimation([
      [
        '  o    ',
        '       ',
        '       ',
        '       '
      ],
      [
        '       ',
        '   o   ',
        '       ',
        '       '
      ],
      [
        '       ',
        '       ',
        '    o  ',
        '       '
      ],
      [
        '       ',
        '       ',
        '       ',
        '     o '
      ],
      [
        '       ',
        '       ',
        '      o',
        '       '
      ],
      [
        '       ',
        '     o ',
        '       ',
        '       '
      ],
      [
        '    o  ',
        '       ',
        '       ',
        '       '
      ],
      [
        '       ',
        '   o   ',
        '       ',
        '       '
      ],
      [
        '       ',
        '       ',
        '  o    ',
        '       '
      ],
      [
        '       ',
        '       ',
        '       ',
        ' o     '
      ],
      [
        '       ',
        '       ',
        'o      ',
        '       '
      ],
      [
        '       ',
        ' o     ',
        '       ',
        '       '
      ]
    ], 8));
  })(jQuery.noConflict());

  window["myterminal-loaded"] = true;


  // Add bottom navigation (next previous post)
  (async () => {
    const isOnPost = !(window.location.pathname.endsWith("index.html") || !window.location.pathname.endsWith(".html"))
    if (!isOnPost) return;

    let sitemap = await (await fetch("/sitemap.json")).json();
    sitemap = sitemap[0].contents;
    let files = getWdNodeType("file")
    let pages = files.filter((name) => name !== "index.html" && typeof (name) === 'string' && name.endsWith(".html"))
      .sort((a, b) => {
        return a.replace(".html", "") - b.replace(".html", "")
      })
    const nextbtn = document.querySelector(".next")
    const prevbtn = document.querySelector(".previous")
    const current_name = window.location.pathname.split("/").slice(-1)[0]
    const current_index = pages.indexOf(current_name)

    if (current_index > 0) {
      prevbtn.style.display = "inline-block"
      prevbtn.href = "./" + pages[current_index - 1]
    }
    if (current_index < (pages.length - 1)) {
      nextbtn.style.display = "inline-block"
      nextbtn.href = "./" + pages[current_index + 1]
    }
  })()

});
