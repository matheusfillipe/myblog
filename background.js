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


function load3dscene(full) {
  scene = new THREE.Scene();
  var camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight);
  var renderer = new THREE.WebGLRenderer({
    canvas: document.querySelector("#bg")
  })
  let controls = null;
  if (full){
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

  let planets = []
  function addPlanet(size, name, rotation, update) {
    const planetTexture = new THREE.TextureLoader().load('/assets/' + name + ".jpg")
    const planet = new THREE.Mesh(
      new THREE.SphereGeometry(size, 32, 32),
      new THREE.MeshStandardMaterial({
        map: planetTexture,
      })
    )
    scene.add(planet)
    planets.push({ planet, rotation, update })
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
    const material = new THREE.MeshPhongMaterial({ color, emissive: color, emissiveIntensity: 20.0})
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
    setTimeout( function() {
        requestAnimationFrame( animate );
    }, 1000 / 30 );

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
  setTimeout( function() {
    scene = null
    window.dispatchEvent(new Event('resize'));
  }, 300 );
}

function reloadScene(...params) {
  disposeScene()
  setTimeout ( function() {
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
$('#terminal').terminal({
    help: function() {
        this.echo(`
THE AVAILABLE COMMANDS ARE
pwd: current directory
ls: List directory
cd: Move to a directory
cat: cat
star: star this project on github
follow: follow me on github
explore: Explore the 3d space (requires FX enabled)
help: shows this help menu
`);
    },
  pwd: function() {
    const title = document.title
    const path = window.location.pathname.toString()
    this.echo($(`<p>${path}</p> <a href="${path}">${title}<a/>`));
    },
  ls: function(path) {
        this.echo($('<img src="https://placekitten.com/408/287">'));
    },
  cd: function(path) {
        this.echo();
    },
  explore: function() {
      document.querySelector("main").remove()
      document.querySelector("#postamble").remove()
      document.querySelector(".title").remove()
      document.querySelector("#disable3dlabel").remove()
      reloadScene({path: true})
      this.echo("I've hidden he boring stuff that was written here\nYou might want to close this window for now or type 'help' to see new commands");
    },
  cat: function() {
        this.echo($('<img src="https://placekitten.com/408/287">'));
    },
  star: function () { this.echo($('<iframe src="https://ghbtns.com/github-btn.html?user=matheusfillipe&repo=myblog&type=star&count=true&size=large" frameborder="0" scrolling="0" width="170" height="30" title="GitHub"></iframe>'))},
  follow: function () {this.echo($('<iframe src="https://ghbtns.com/github-btn.html?user=matheusfillipe&type=follow&count=true&size=large" frameborder="0" scrolling="0" width="230" height="30" title="GitHub"></iframe>'))},
  why: function () {this.echo("Why not?")}
}, {
  greetings: `
   ____ ___  ____ _/ /_/ /____  _________ ___  (_)___  ____ _/ /
  / __ \`__ \\/ __ \`/ __/ __/ _ \\/ ___/ __ \`__ \\/ / __ \\/ __ \`/ /
 / / / / / / /_/ / /_/ /_/  __/ /  / / / / / / / / / / /_/ / /
/_/ /_/ /_/\\__,_/\\__/\\__/\\___/_/  /_/ /_/ /_/_/_/ /_/\\__,_/_/

WELCOME TO THE TERMINAL
Type 'help' to see available commands
`
});
