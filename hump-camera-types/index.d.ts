/**
 * hump/camera.d.ts
 * @description Typecript declarations of hump/camera.lua
 * @version master@08937cc0ecf72d1a964a8de6cd552c5e136bf0d4
 * @author Nyaacinth
 * @license MIT
 */

import { HumpCamera } from "./HumpCamera"

interface IHumpCameraConstructor {
    /**
     * Hump Camera Constructor
     * @param x Point for the camera to look at (X-axis)
     * @param y Point for the camera to look at (Y-axis)
     * @param zoom_factor Camera zoom factor
     * @param rotation Camera rotation (in radians)
     */
    (this: void, x?: number, y?: number, zoom_factor?: number, rotation?: number): HumpCamera
    new: IHumpCameraConstructor
}

/**
 * Hump Camera
 * @description **hump** is a small collection of tools for developing games with LÖVE
 * @link https://github.com/vrld/hump
 * @version master@08937cc0ecf72d1a964a8de6cd552c5e136bf0d4
 * @author vrld
 * @license MIT
 */
declare let camera: IHumpCameraConstructor

export = camera
