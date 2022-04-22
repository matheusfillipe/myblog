// Check 3D toggler
import * as THREE from 'three';
import { OrbitControls } from 'OrbitControls';

function setCookie(name, value, days) {
  var expires = "";
  if (days) {
    var date = new Date();
    date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
    expires = "; expires=" + date.toUTCString();
  }
  document.cookie = name + "=" + (value || "") + expires + "; path=/; Secure";
}

function getCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for (var i = 0; i < ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0) == ' ') c = c.substring(1, c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
  }
  return null;
}

function eraseCookie(name) {
  document.cookie = name + '=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT; Secure';
}
var isMobile = false; //initiate as false
// device detection
if (/(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|ipad|iris|kindle|Android|Silk|lge |maemo|midp|mmp|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows (ce|phone)|xda|xiino/i.test(navigator.userAgent)
  || /1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i.test(navigator.userAgent.substr(0, 4))) {
  isMobile = true;
}

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
    planets.push({ planet, rotation, update, radius: size})
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
    setTimeout(function() {
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
  setTimeout(function() {
    scene = null
    window.dispatchEvent(new Event('resize'));
  }, 300);
}

function reloadScene(...params) {
  disposeScene()
  setTimeout(function() {
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
if ((isMobile && (!load3d || load3d !== "true")) || (load3d && load3d === "false")) {
  toggler.checked = false
} else {
  enablefx()
}


// TERMINAL

//load sitemap for ls and cd commands
document.addEventListener("DOMContentLoaded", async function() {
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
  (function($) {
    $('#terminal').terminal({
      help: function() {
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
  help: shows this help menu
  `);
      },
      search: async function(query) {

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
      pwd: function() {
        const title = document.title
        const path = window.location.pathname.toString()
        const wds = "/" + wd.slice(1).join("/") + "/"
        this.echo($(`<p>${path} <a href="${path}">${title}<a/><p/><p>${wds}</p>`));
      },
      ls: function() {
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
      cd: function(path) {
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
      open: function(file) {
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
      ping: function() {
        this.echo("pong\n")
      },
      apt: function() {
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
      pacman: function() {
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
      gasconheart: function() {
        this.echo("nc mangle.ga 8888")
      },
      explore: function() {
        document.querySelector("main").remove()
        document.querySelector("#postamble").remove()
        document.querySelector(".title").remove()
        document.querySelector("#disable3dlabel").remove()
        reloadScene({ path: true })
        this.echo("I've hidden he boring stuff that was written here\nYou might want to close this window for now or type 'help' to see new commands");
      },
      torus: function() {
        planets.forEach((p) => {
          if (p.planet.geometry.type === "TorusGeometry") {
            return
          }
          const radius = p.planet.geometry.boundingSphere.radius
          p.planet.geometry.dispose();
          const geometry = new THREE.TorusGeometry(3 * radius, radius, 16, 100)
          p.planet.geometry = geometry
          p.rotation[2] = p.rotation[1] * 2
        })
      },
      sphere: function() {
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
      cat: function() {
        this.echo($('<img src="https://placekitten.com/408/287">'));
      },
      star: function() { this.echo($('<iframe src="https://ghbtns.com/github-btn.html?user=matheusfillipe&repo=myblog&type=star&count=true&size=large" frameborder="0" scrolling="0" width="170" height="30" title="GitHub"></iframe>')) },
      follow: function() { this.echo($('<iframe src="https://ghbtns.com/github-btn.html?user=matheusfillipe&type=follow&count=true&size=large" frameborder="0" scrolling="0" width="230" height="30" title="GitHub"></iframe>')) },
      why: function() { this.echo("Why not?") }
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
      completion: function(string, callback) {
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
      keydown: function(e, term) {
        if (e.which === 68 && e.ctrlKey) { // CLTR+D
          document.querySelector("#terminalwindow").classList.add("terminal--hidden")
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
});
