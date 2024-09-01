import util from 'node:util';

function functionToString(func) {
  return `Function: ${func.name || 'anonymous'}\nSource: ${func.toString()}`;
}

function deepInspect(value, depth = 0) {
  if (typeof value === 'function') {
    return functionToString(value);
  }
  if (typeof value === 'object' && value !== null) {
    if (Array.isArray(value)) {
      return value.map(item => deepInspect(item, depth + 1));
    }
    const inspectedObject = {};
    for (const [key, val] of Object.entries(value)) {
      inspectedObject[key] = deepInspect(val, depth + 1);
    }
    return inspectedObject;
  }
  return value;
}

function log(...args) {
  // console.log('-----------------------');
  // const formatted = args.map(arg => {
  //   const inspected = deepInspect(arg);
  //   return util.inspect(inspected, { showHidden: false, depth: null, colors: true });
  // }).join('\n');
  // console.log(formatted);
  // console.log('-----------------------');
}

/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////

// Initializes a new object with a tag and properties to manage a sequence of subscriptions and function calls.
export const objHack = (tag) => () => ({
  isRunning: false,       // Flag to track if the object is currently processing subscriptions.
  queue: [],              // Queue to hold functions that need to be processed sequentially.
  subscriptions: [{}],        // Array of objects, each representing a set of subscriptions.
  tag,                    // A tag or identifier for the object.
});

// Inserts a key-value pair into the latest subscription object in the subscriptions array.
export const insertObjHack = (key, value, obj) => {
  log("insertObjHack", {key, value, obj})

  // Note: We do not push a new subscription object here because this function
  // is only responsible for adding key-value pairs to the most recent subscription.
  const latestSubscription = obj.subscriptions[obj.subscriptions.length - 1];
  latestSubscription[key] = value;  // key = "0"
  log("insertObjHack END", {key, value, obj})
};

// Deletes a key from any subscription object in the subscriptions array.
export const deleteObjHack = (key, obj) => {
  log("deleteObjHack", {key, obj})

  for (const subscription of obj.subscriptions) {
    if (delete subscription[key]) { // If the key exists and is deleted, return true.
      log("deleteObjHack END", {key, obj})
      return true;
    }
  }
  log("deleteObjHack END", {key, obj})
  return false; // Return false if the key is not found in any subscription.
};

// Recursively applies a function to each value in the subscriptions array.
const constructResult = (subscriptions, newSubscriptions, fn, index) => {
  log("constructResult", {subscriptions, newSubscriptions, fn, index})

  subscriptions.push({});  // Start a new subscription stage.

  // Iterate over each key-value pair in the current subscription stage.
  for (const [key, value] of Object.entries(subscriptions[index])) {
    fn(value);  // Apply the function to the current value.

    // If there are any subscriptions in the next stage, recursively apply them.
    if (Object.keys(subscriptions[index + 1]).length > 0) {
      constructResult(subscriptions, newSubscriptions, fn, index + 1);
    }

    subscriptions[index + 1] = {};  // Reset the next subscription stage.
    subscriptions.length = index + 2;  // Ensure subscriptions array has only the current and next stage.
    newSubscriptions[key] = value;  // Update the newSubscriptions object with the final value of the subscription.
  }
  log("constructResult END", {subscriptions, newSubscriptions, fn, index})
};

// Processes the queue of functions, applying each to the object in sequence.
export const fastForeachOhE = (obj, fn) => {
  log("fastForeachOhE", {obj, fn})

  let currentFn = fn;

  while (true) {
    if (obj.isRunning) {  // If the object is already processing, queue the function and return.
      obj.queue.push(currentFn);
      log("fastForeachOhE", {obj, fn})
      return;
    }

    obj.isRunning = true;  // Mark the object as running to prevent re-entry.
    const newSubscriptions = {};
    constructResult(obj.subscriptions, newSubscriptions, currentFn, 0);  // Apply all subscriptions starting from index 0.
    // console.log("I built M", JSON.stringify({ M, o }, null, 2))

    obj.subscriptions.length = 0;  // Clear all subscriptions after processing.
    obj.subscriptions.push(newSubscriptions);  // Start a new subscription with the final newSubscriptions.
    obj.isRunning = false;  // Mark the object as not running, allowing new functions to be processed.

    currentFn = obj.queue.shift();  // Get the next function in the queue.
    if (currentFn === undefined) {  // Exit the loop if no more functions are left.
      break;
    }
  }
  log("fastForeachOhE END", {obj, fn})
};

// const objHack1 = objHack("")()
// insertObjHack(0, (x) => console.log("asdf", x), objHack1)
// console.log(objHack1)
// fastForeachOhE(objHack1, (value) => {
//   console.log("level1", value)
// })
// console.log(objHack1)
// console.log("---------------------------")

// const objHack2 = objHack("")()
// insertObjHack(0, (x) => console.log("asdf", x), objHack2)
// insertObjHack(1, (x) => console.log("asdf2", x), objHack2)
// console.log(objHack2)
// fastForeachOhE(objHack2, (value) => {
//   console.log("level1", value)
// })
// console.log(objHack2)
// console.log("---------------------------")

// const objHack3 = objHack("")()
// insertObjHack(0, (x) => console.log("asdf", x), objHack3)
// // insertObjHack(1, (x) => console.log("asdf", x), objHack3)
// console.log(objHack3)
// fastForeachOhE(objHack3, (value) => {
//   console.log("level1", value)
//   fastForeachOhE(objHack3, (value) => {
//     console.log("level2", value)
//   })
// })
// console.log(objHack3)
// console.log("---------------------------")
