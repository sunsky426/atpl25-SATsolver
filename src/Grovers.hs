module Grovers where

import Gates

-- diffussion step (reflect across the equal superposition vector)
diffusion :: CircuitWidth -> Program
diffusion width = pow H width ++ pow X width ++ [MCZ [0..width-1]] ++ pow X width ++ pow H width

grovers :: CircuitWidth -> Program -> Int -> Program -- groversCuircuit that does infinite iterations, just take as much as you need
grovers width oracle iterations = pow H width ++ concat (replicate iterations (oracle ++ diffusion width))