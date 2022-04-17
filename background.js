import * as THREE from 'three';
import { OrbitControls } from 'OrbitControls';

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight);
// const renderer = new THREE.WebGLRenderer({
//   canvas: document.querySelector("#bg")
// })
