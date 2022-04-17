import * as THREE from 'three';
import { OrbitControls } from 'OrbitControls';

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight);
const renderer = new THREE.WebGLRenderer({
  canvas: document.querySelector("#bg")
})

renderer.setPixelRatio(window.devicePixelRatio)
renderer.setSize(window.innerWidth, window.innerHeight)

camera.position.setZ(30)
const initialPos = camera.position


const geometry = new THREE.TorusGeometry(10, 3, 16, 100)
// const material = new THREE.MeshBasicMaterial({color: 0xFF6347, wireframe: true})
const material = new THREE.MeshStandardMaterial({color: 0x3F88C5})
const torus = new THREE.Mesh(geometry, material)
scene.add(torus)

const pointLight = new THREE.PointLight(0xfffffff)
pointLight.position.set(20, 20, 20)
scene.add(pointLight)

const ambientLight = new THREE.AmbientLight(0xfffffff, 0.5)
scene.add(ambientLight)

const controls = new OrbitControls(camera, renderer.domElement)

function addStar() {
  const geometry = new THREE.SphereGeometry(0.15, 24, 24)
  const material = new THREE.MeshBasicMaterial({color: 0xffffff})
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
  camera.position.z = t * -0.0001 + initialPos.z;
  camera.position.x = t * -0.0002 + initialPos.x;
  camera.position.y = t * -0.0002 + initialPos.y;
}

document.body.onscroll = scrollAnimate;




function animate() {
  requestAnimationFrame(animate);
  renderer.render(scene, camera)
  torus.rotation.x += 0.01;
  torus.rotation.y += 0.05;
  torus.rotation.z += 0.01;

  controls.update();
}

animate()
