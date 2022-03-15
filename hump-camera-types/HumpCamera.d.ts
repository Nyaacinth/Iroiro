import { HumpCameraSmoother } from "./HumpCameraSmoother"

/** Hump Camera Object */
export type HumpCamera = {
    /**
     * Move the camera by some vector
     * @param dx Direction to move the camera (X-axis)
     * @param dy Direction to move the camera (Y-axis)
     */
    move(dx: number, dy: number): HumpCamera

    /**
     * Set the camera position
     * @param x Position to look at (X-axis)
     * @param y Position to look at (Y-axis)
     */
    lookAt(x: number, y: number): HumpCamera

    /**
     * Returns camera.x, camera.y
     * @returns [1] — `x` camera.x
     * @returns [2] — `y` camera.y
     */
    position(): LuaMultiReturn<[number, number]>

    /**
     * Rotate the camera by some angle
     * @param angle Rotation angle in radians
     */
    rotate(angle: number): HumpCamera

    /**
     * Set rotation
     * @param angle Rotation angle in radians
     */
    rotateTo(angle: number): HumpCamera

    /**
     * Multiply zoom factor
     * @param mul  Zoom change (mul > 0)
     */
    zoom(mul: number): HumpCamera

    /**
     * Set zoom factor
     * @param zoom New zoom factor
     */
    zoomTo(zoom: number): HumpCamera

    /** Apply camera transformations until `detach()` */
    attach(): void

    /** Unset camera transformations */
    detach(): void

    /**
     * Wrap a function between an `attach()`/`detach()` pair
     * @param func Drawing function to be wrapped
     */
    draw(func: (this: void) => void): void

    /**
     * Transform point from camera coordinates to world coordinates
     * @param x Point to transform (X-axis)
     * @param y Point to transform (Y-axis)
     * @returns x, Transformed point (X-axis)
     * @returns y, Transformed point (Y-axis)
     */
    worldCoords(x: number, y: number): LuaMultiReturn<[number, number]>

    /**
     * Transform point from world coordinates to camera coordinates
     * @param x Point to transform (X-axis)
     * @param y Point to transform (Y-axis)
     * @returns x, Transformed point (X-axis)
     * @returns y, Transformed point (Y-axis)
     */
    cameraCoords(x: number, y: number): LuaMultiReturn<[number, number]>

    /**
     * Shortcut to `worldCoords(love.mouse.getPosition())`
     * @returns x, Transformed point (X-axis)
     * @returns y, Transformed point (Y-axis)
     */
    mousePosition(): LuaMultiReturn<[number, number]>

    /**
     * Horizontal camera locking
     * @param x X coordinate (in world coordinates)
     * @param smoother Movement smoothing override
     * @param vargs Additional parameters to the smoothing function
     */
    lockX(x: number, smoother?: HumpCameraSmoother, ...vargs: any[]): void

    /**
     * Vertical camera locking
     * @param y Y coordinate (in world coordinates)
     * @param smoother Movement smoothing override
     * @param vargs Additional parameters to the smoothing function
     */
    lockY(y: number, smoother?: HumpCameraSmoother, ...vargs: any[]): void

    /**
     * Horizontal and vertical camera locking
     * @param x Position X-axis (in world coordinates)
     * @param y Position Y-axis (in world coordinates)
     * @param smoother Movement smoothing override
     * @param vargs Additional parameters to the smoothing function
     */
    lockPosition(x: number, y: number, smoother?: HumpCameraSmoother, ...vargs: any[]): void

    /**
     * Lock camera to x,y, but only move the camera if the position would be out of the screen-rectangle defined by `x_min`, `x_max`, `y_min`, `y_max`
     * @param x Target Position X-axis (in world coordinates)
     * @param y Target Position Y-axis (in world coordinates)
     * @param x_min Upper left X coordinate of the camera window (in camera coordinates)
     * @param x_max Lower right X coordinate of the camera window (in camera coordinates)
     * @param y_min Upper left Y coordinate of the camera window (in camera coordinates)
     * @param y_max Lower right Y coordinate of the camera window (in camera coordinates)
     * @param smoother Movement smoothing override
     * @param vargs Additional parameters to the smoothing function
     */
    lockWindow(x: number, y: number, x_min: number, x_max: number, y_min: number, y_max: number, smoother?: HumpCameraSmoother, ...vargs: any[]): void

    /** Default Smoother Collection */
    smooth: {
        none(): HumpCameraSmoother
        linear(speed: number): HumpCameraSmoother
        damped(stiffness: number): HumpCameraSmoother
    }
}
