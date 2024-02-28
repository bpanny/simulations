let width = 400;
let angle = 0; // Initialize angle to control the fade effect
let browAngle = 0; // Initialize browAngle for eyebrow movement
let gif;
let canvas;

function setup() {
    canvas = createCanvas(400, 400);
    background(220);
}

function draw() {

  fill(255, 255, 255);
  stroke(0, 0, 0);
  circle(200, 200, 200); // Face
  fill(0, 0, 0);
  noFill();
  stroke(0, 0, 0);
  arc(200, 220, 120, 100, 0, PI, OPEN); // Mouth
  arc(165, 155, 45, 20, 0, PI, OPEN); // Eye socket
  arc(165, 155, 45, 20, PI, 0, OPEN); // Eye socket
  arc(235, 155, 45, 20, 0, PI, OPEN); // Eye socket
  arc(235, 155, 45, 20, PI, 0, OPEN); // Eye socket
  noStroke(); // Optional: Remove if you want an outline for the blush

  // Dynamic elements for blushing
  let alpha = map(sin(angle), -1, 1, 0, 200); // Map the sin value to alpha range
  fill(255, 0, 127, alpha); // Use the alpha for the red fill
  circle(130, 195, 30); // Left blush
  circle(270, 195, 30); // Right blush
  angle += 0.02; // Adjust this value if the fade is too fast or too slow
  
  fill(153, 76, 0, 180);
  circle(165, 155, 20); // Left eye
  circle(235, 155, 20); // Right eye
  fill(0, 0, 0);
  circle(165, 155, 10); // Left pupil
  circle(235, 155, 10); // Right pupil
  
  // Eyebrow movement
  let browLift = map(sin(browAngle), -1, 1, 0, 10); // Map the sin value to eyebrow movement range
  stroke(0, 0, 0);
  arc(165, 140 - browLift, 45, 10, PI, 0, OPEN); // Left eyebrow
  arc(235, 140 - browLift, 45, 10, PI, 0, OPEN); // Right eyebrow
  browAngle += 0.1; // Increase this value for quicker movement

  noStroke(); // Add this if you don't want an outline around the blush
}
