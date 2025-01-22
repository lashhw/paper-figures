function [a_opt, b_opt, c_opt, f_opt, exitflag] = solve_fmincon(k, a_max, b_max, c_max)
% Maximize f(a,b,c) subject to g(a,b,c)=k and bound constraints
%   - f(a,b,c) = a*b*c
%   - g(a,b,c) = a*b + (a+b)*c
%
%   - k: number of memory read
%   - a_max, b_max, c_max: upper bounds for a, b, c
%   - returns:
%       a_opt, b_opt, c_opt, f_opt : the optimal a, b, c, f(a,b,c)
%       exitflag                   : fmincon exit code (1 = success, etc.)
%
%   We use fmincon to minimize -f(a,b,c) subject to:
%     1 <= a <= a_max,
%     1 <= b <= b_max,
%     1 <= c <= c_max,
%     g(a,b,c) = k.

    % Lower and upper bounds for [a, b, c]
    lb = [1, 1, 1];
    ub = [a_max, b_max, c_max];

    % Objective function for minimization (i.e. negative of a*b*c)
    objFun = @(x) - (x(1) * x(2) * x(3));

    % Nonlinear constraint function:
    % We have no inequality constraints beyond simple bounds,
    % but we do have 1 equality: a*b + (a+b)*c = k.
    function [cineq, ceq] = myNonlinCon(x)
        a = x(1); b = x(2); c = x(3);
        cineq = [];  % no extra inequalities
        ceq   = b*c + (b+c)*a - k;  % must be zero
    end

    % Initial guess (some reasonable guess inside bounds)
    x0 = [ 1, 1, max(1, (k-1)/2) ];

    % Options for fmincon
    options = optimoptions('fmincon','Display','none','Algorithm','sqp');

    % Call fmincon
    [xsol, fval, exitflag] = fmincon(objFun, x0, ...
                                     [],[],[],[], ...  % no linear constraints
                                     lb, ub, ...
                                     @myNonlinCon, ...
                                     options);

    % Extract results
    a_opt = xsol(1);
    b_opt = xsol(2);
    c_opt = xsol(3);
    % Since we minimized -a*b*c, the maximum product is -fval:
    f_opt = -fval;
end

% Example usage
k_list = 1:100:400000;

a_max_choices = 5;
a_max_step = 25;
a_max_values = a_max_step:a_max_step:(a_max_choices*a_max_step);

b_max = 320;
c_max = 768;

% Preallocate results
results = struct('a_max', num2cell(a_max_values), 'data', cell(1, a_max_choices));

% Iterate over a_max values
for idx = 1:length(results)
    a_max = results(idx).a_max;
    temp_results = zeros(length(k_list), 4);  % [a, b, c, f]
    for i = 1:length(k_list)
        k = k_list(i);
        [a_opt, b_opt, c_opt, f_opt, exitflag] = solve_fmincon(k, a_max, b_max, c_max);
        if exitflag > 0
            temp_results(i,:) = [a_opt, b_opt, c_opt, f_opt];
        else
            temp_results(i,:) = [NaN, NaN, NaN, NaN];
        end
    end
    results(idx).data = temp_results;
end

% Plot k vs. f
figure;
for idx = 1:length(results)
    plot(k_list, results(idx).data(:,4), 'o-', 'DisplayName', sprintf('a_{max} = %d', results(idx).a_max));
    hold on;
end
xlabel('k'); ylabel('max f(a,b,c)'); grid on;
legend show;
legend('Location', 'northwest');
hold off;

% Plot ratio of f(a,b,c) relative to the first line
figure;
for idx = 1:length(results)
    ratio = results(idx).data(:,4) ./ results(1).data(:,4);
    plot(k_list, ratio, 'o-', 'DisplayName', sprintf('a_{max} = %d / a_{max} = %d', results(idx).a_max, results(1).a_max));
    hold on;
end
xlabel('k'); ylabel('max f(a,b,c) ratio'); grid on;
legend show;
legend('Location', 'northwest');
hold off;

% Plot 3D optimal (a,b,c) with gradient color for k
figure;
colormap(flipud(jet));
for idx = 1:length(results)
    scatter3(results(idx).data(:,1), results(idx).data(:,2), results(idx).data(:,3), 36, k_list, 'filled');
    hold on;
end
colorbar;
xlabel('a'); ylabel('b'); zlabel('c'); grid on;
title('optimal (a,b,c) subject to the constraints');
hold off;
